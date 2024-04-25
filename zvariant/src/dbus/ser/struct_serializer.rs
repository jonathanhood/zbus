use super::Serializer;
use crate::{
    container_depths::ContainerDepths, serialized::Format, signature_parser::SignatureParser,
    utils::*, Error, Result,
};
use serde::{ser, Serialize};
use std::{
    io::{Seek, Write},
    str,
};

#[doc(hidden)]
pub struct StructSerializer<'ser, 'sig, 'b, W> {
    ser: &'b mut Serializer<'ser, 'sig, W>,
    // The number of `)` in the signature to skip at the end.
    end_parens: u8,
    // The original container depths. We restore to that at the end.
    container_depths: ContainerDepths,
}

impl<'ser, 'sig, 'b, W> StructSerializer<'ser, 'sig, 'b, W>
where
    W: Write + Seek,
{
    pub(super) fn variant(ser: &'b mut Serializer<'ser, 'sig, W>) -> Result<Self> {
        ser.0.add_padding(VARIANT_ALIGNMENT_DBUS)?;
        let container_depths = ser.0.container_depths;
        ser.0.container_depths = ser.0.container_depths.inc_variant()?;

        Ok(Self {
            ser,
            end_parens: 0,
            container_depths,
        })
    }

    pub(super) fn structure(ser: &'b mut Serializer<'ser, 'sig, W>) -> Result<Self> {
        let c = ser.0.sig_parser.next_char()?;
        if c != STRUCT_SIG_START_CHAR && c != DICT_ENTRY_SIG_START_CHAR {
            let expected = format!("`{STRUCT_SIG_START_STR}` or `{DICT_ENTRY_SIG_START_STR}`",);

            return Err(serde::de::Error::invalid_type(
                serde::de::Unexpected::Char(c),
                &expected.as_str(),
            ));
        }

        let signature = ser.0.sig_parser.next_signature()?;
        let alignment = alignment_for_signature(&signature, Format::DBus)?;
        ser.0.add_padding(alignment)?;

        ser.0.sig_parser.skip_char()?;
        let container_depths = ser.0.container_depths;
        ser.0.container_depths = ser.0.container_depths.inc_structure()?;

        Ok(Self {
            ser,
            end_parens: 1,
            container_depths,
        })
    }

    pub(super) fn unit(ser: &'b mut Serializer<'ser, 'sig, W>) -> Result<Self> {
        // serialize as a `0u8`
        serde::Serializer::serialize_u8(&mut *ser, 0)?;

        let container_depths = ser.0.container_depths;
        Ok(Self {
            ser,
            end_parens: 0,
            container_depths,
        })
    }

    pub(super) fn enum_variant(ser: &'b mut Serializer<'ser, 'sig, W>) -> Result<Self> {
        let mut ser = Self::structure(ser)?;
        ser.end_parens += 1;

        Ok(ser)
    }

    pub(super) fn serialize_struct_element<T>(
        &mut self,
        name: Option<&'static str>,
        value: &T,
    ) -> Result<()>
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

                let sig_parser = SignatureParser::new(signature);
                let bytes_written = self.ser.0.bytes_written;
                let mut ser = Serializer(crate::SerializerCommon::<W> {
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

                Ok(())
            }
            _ => value.serialize(&mut *self.ser),
        }
    }

    pub(super) fn end_struct(self) -> Result<()> {
        if self.end_parens > 0 {
            self.ser.0.sig_parser.skip_chars(self.end_parens as usize)?;
        }
        // Restore the original container depths.
        self.ser.0.container_depths = self.container_depths;

        Ok(())
    }
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

serialize_struct_anon_fields!(SerializeTuple serialize_element);
serialize_struct_anon_fields!(SerializeTupleStruct serialize_field);
serialize_struct_anon_fields!(SerializeTupleVariant serialize_field);
serialize_struct_named_fields!(SerializeStruct);
serialize_struct_named_fields!(SerializeStructVariant);
