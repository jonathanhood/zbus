use serde::{ser, Serialize};
use std::{
    io::{Seek, Write},
    str,
};

use crate::{
    container_depths::ContainerDepths, framing_offsets::FramingOffsets, serialized::Format,
    signature_parser::SignatureParser, utils::*, Error, Result,
};

#[doc(hidden)]
pub struct StructSerializer<'ser, 'sig, 'b, W> {
    ser: &'b mut super::Serializer<'ser, 'sig, W>,
    start: usize,
    // The number of `)` in the signature to skip at the end.
    end_parens: u8,
    // All offsets
    offsets: Option<FramingOffsets>,
    // The original container depths. We restore to that at the end.
    container_depths: ContainerDepths,
}

impl<'ser, 'sig, 'b, W> StructSerializer<'ser, 'sig, 'b, W>
where
    W: Write + Seek,
{
    pub(super) fn variant(ser: &'b mut super::Serializer<'ser, 'sig, W>) -> Result<Self> {
        ser.0.add_padding(VARIANT_ALIGNMENT_GVARIANT)?;
        let offsets = if ser.0.sig_parser.next_char()? == STRUCT_SIG_START_CHAR {
            Some(FramingOffsets::new())
        } else {
            None
        };
        let start = ser.0.bytes_written;
        let container_depths = ser.0.container_depths;
        ser.0.container_depths = ser.0.container_depths.inc_variant()?;

        Ok(Self {
            ser,
            end_parens: 0,
            offsets,
            start,
            container_depths,
        })
    }

    pub(super) fn structure(ser: &'b mut super::Serializer<'ser, 'sig, W>) -> Result<Self> {
        let c = ser.0.sig_parser.next_char()?;
        if c != STRUCT_SIG_START_CHAR && c != DICT_ENTRY_SIG_START_CHAR {
            let expected = format!("`{STRUCT_SIG_START_STR}` or `{DICT_ENTRY_SIG_START_STR}`",);

            return Err(serde::de::Error::invalid_type(
                serde::de::Unexpected::Char(c),
                &expected.as_str(),
            ));
        }

        let signature = ser.0.sig_parser.next_signature()?;
        let alignment = alignment_for_signature(&signature, Format::GVariant)?;
        ser.0.add_padding(alignment)?;

        ser.0.sig_parser.skip_char()?;

        let offsets = if c == STRUCT_SIG_START_CHAR {
            Some(FramingOffsets::new())
        } else {
            None
        };
        let start = ser.0.bytes_written;
        let container_depths = ser.0.container_depths;
        ser.0.container_depths = ser.0.container_depths.inc_structure()?;

        Ok(Self {
            ser,
            end_parens: 1,
            offsets,
            start,
            container_depths,
        })
    }

    pub(super) fn unit(ser: &'b mut super::Serializer<'ser, 'sig, W>) -> Result<Self> {
        // serialize as a `0u8`
        serde::Serializer::serialize_u8(&mut *ser, 0)?;

        let start = ser.0.bytes_written;
        let container_depths = ser.0.container_depths;
        Ok(Self {
            ser,
            end_parens: 0,
            offsets: None,
            start,
            container_depths,
        })
    }

    pub(super) fn enum_variant(ser: &'b mut super::Serializer<'ser, 'sig, W>) -> Result<Self> {
        let mut ser = Self::structure(ser)?;
        ser.end_parens += 1;

        Ok(ser)
    }

    fn serialize_struct_element<T>(&mut self, name: Option<&'static str>, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        match name {
            Some("zvariant::Value::Value") => {
                // Serializing the value of a Value, which means signature was serialized
                // already, and also put aside for us to be picked here.
                let signature = self
                    .ser
                    .0
                    .value_sign
                    .take()
                    .expect("Incorrect Value encoding");

                let sig_parser = SignatureParser::new(signature.clone());
                let bytes_written = self.ser.0.bytes_written;
                let mut ser = super::Serializer(crate::SerializerCommon::<W> {
                    ctxt: self.ser.0.ctxt,
                    sig_parser,
                    writer: self.ser.0.writer,
                    #[cfg(unix)]
                    fds: self.ser.0.fds,
                    bytes_written,
                    value_sign: None,
                    container_depths: self.ser.0.container_depths,
                });
                value.serialize(&mut ser)?;
                self.ser.0.bytes_written = ser.0.bytes_written;

                self.ser
                    .0
                    .write_all(&b"\0"[..])
                    .map_err(|e| Error::InputOutput(e.into()))?;
                self.ser
                    .0
                    .write_all(signature.as_bytes())
                    .map_err(|e| Error::InputOutput(e.into()))?;

                Ok(())
            }
            _ => {
                let element_signature = self.ser.0.sig_parser.next_signature()?;
                let fixed_sized_element =
                    crate::utils::is_fixed_sized_signature(&element_signature)?;

                value.serialize(&mut *self.ser)?;

                if let Some(ref mut offsets) = self.offsets {
                    if !fixed_sized_element {
                        offsets.push_front(self.ser.0.bytes_written - self.start);
                    }
                }

                Ok(())
            }
        }
    }

    pub(super) fn end_struct(self) -> Result<()> {
        if self.end_parens > 0 {
            self.ser.0.sig_parser.skip_chars(self.end_parens as usize)?;
        }
        // Restore the original container depths.
        self.ser.0.container_depths = self.container_depths;

        let mut offsets = match self.offsets {
            Some(offsets) => offsets,
            None => return Ok(()),
        };
        let struct_len = self.ser.0.bytes_written - self.start;
        if struct_len == 0 {
            // Empty sequence
            return Ok(());
        }
        if offsets.peek() == Some(struct_len) {
            // For structs, we don't want offset of last element
            offsets.pop();
        }

        offsets.write_all(&mut self.ser.0, struct_len)?;

        Ok(())
    }
}

macro_rules! serialize_struct_named_fields {
    ($trait:ident) => {
        impl<'ser, 'sig, 'b, W> ser::$trait for StructSerializer<'ser, 'sig, 'b, W>
        where
            W: Write + Seek,
        {
            type Ok = ();
            type Error = Error;

            fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
            where
                T: ?Sized + Serialize,
            {
                self.serialize_struct_element(Some(key), value)
            }

            fn end(self) -> Result<()> {
                self.end_struct()
            }
        }
    };
}

macro_rules! serialize_struct_anon_fields {
    ($trait:ident $method:ident) => {
        impl<'ser, 'sig, 'b, W> ser::$trait for StructSerializer<'ser, 'sig, 'b, W>
        where
            W: Write + Seek,
        {
            type Ok = ();
            type Error = Error;

            fn $method<T>(&mut self, value: &T) -> Result<()>
            where
                T: ?Sized + Serialize,
            {
                self.serialize_struct_element(None, value)
            }

            fn end(self) -> Result<()> {
                self.end_struct()
            }
        }
    };
}
serialize_struct_anon_fields!(SerializeTuple serialize_element);
serialize_struct_anon_fields!(SerializeTupleStruct serialize_field);
serialize_struct_anon_fields!(SerializeTupleVariant serialize_field);
serialize_struct_named_fields!(SerializeStruct);
serialize_struct_named_fields!(SerializeStructVariant);
