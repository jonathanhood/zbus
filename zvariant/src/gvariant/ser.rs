mod seq_serializer;
mod struct_seq_serializer;
mod struct_serializer;

use serde::{ser, ser::SerializeSeq, Serialize};
use static_assertions::assert_impl_all;
use std::{
    io::{Seek, Write},
    str,
};

use crate::{
    framing_offsets::FramingOffsets,
    serialized::{Context, Format},
    signature_parser::SignatureParser,
    utils::*,
    Basic, Error, Result, Signature,
};

use self::{
    seq_serializer::SeqSerializer, struct_seq_serializer::StructSeqSerializer,
    struct_serializer::StructSerializer,
};

/// Our serialization implementation.
pub(crate) struct Serializer<'ser, 'sig, W>(pub(crate) crate::SerializerCommon<'ser, 'sig, W>);

assert_impl_all!(Serializer<'_, '_, i32>: Send, Sync, Unpin);

impl<'ser, 'sig, W> Serializer<'ser, 'sig, W>
where
    W: Write + Seek,
{
    /// Create a GVariant Serializer struct instance.
    ///
    /// On Windows, the method doesn't have `fds` argument.
    pub fn new<'w: 'ser, 'f: 'ser, S>(
        signature: S,
        writer: &'w mut W,
        #[cfg(unix)] fds: &'f mut crate::ser::FdList,
        ctxt: Context,
    ) -> Result<Self>
    where
        S: TryInto<Signature<'sig>>,
        S::Error: Into<Error>,
    {
        assert_eq!(ctxt.format(), Format::GVariant);

        let signature = signature.try_into().map_err(Into::into)?;
        let sig_parser = SignatureParser::new(signature);
        Ok(Self(crate::SerializerCommon {
            ctxt,
            sig_parser,
            writer,
            #[cfg(unix)]
            fds,
            bytes_written: 0,
            value_sign: None,
            container_depths: Default::default(),
        }))
    }

    #[cfg(not(feature = "option-as-array"))]
    fn serialize_maybe<T>(&mut self, value: Option<&T>) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let signature = self.0.sig_parser.next_signature()?;
        let alignment = alignment_for_signature(&signature, self.0.ctxt.format())?;
        let child_sig_parser = self.0.sig_parser.slice(1..);
        let child_signature = child_sig_parser.next_signature()?;
        let child_sig_len = child_signature.len();
        let fixed_sized_child = crate::utils::is_fixed_sized_signature(&child_signature)?;

        self.0.sig_parser.skip_char()?;

        self.0.add_padding(alignment)?;

        match value {
            Some(value) => {
                self.0.container_depths = self.0.container_depths.inc_maybe()?;
                value.serialize(&mut *self)?;
                self.0.container_depths = self.0.container_depths.dec_maybe();

                if !fixed_sized_child {
                    self.0
                        .write_all(&b"\0"[..])
                        .map_err(|e| Error::InputOutput(e.into()))?;
                }
            }
            None => {
                self.0.sig_parser.skip_chars(child_sig_len)?;
            }
        }

        Ok(())
    }
}

macro_rules! serialize_basic {
    ($method:ident, $type:ty) => {
        fn $method(self, v: $type) -> Result<()> {
            let ctxt = Context::new_dbus(self.0.ctxt.endian(), self.0.ctxt.position());
            let bytes_written = self.0.bytes_written;
            let mut dbus_ser = crate::dbus::Serializer(crate::SerializerCommon::<W> {
                ctxt,
                sig_parser: self.0.sig_parser.clone(),
                writer: &mut self.0.writer,
                #[cfg(unix)]
                fds: self.0.fds,
                bytes_written,
                value_sign: None,
                container_depths: self.0.container_depths,
            });

            dbus_ser.$method(v)?;

            self.0.bytes_written = dbus_ser.0.bytes_written;
            self.0.sig_parser = dbus_ser.0.sig_parser;

            Ok(())
        }
    };
}

impl<'ser, 'sig, 'b, W> ser::Serializer for &'b mut Serializer<'ser, 'sig, W>
where
    W: Write + Seek,
{
    type Ok = ();
    type Error = Error;

    type SerializeSeq = SeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeTuple = StructSeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeTupleStruct = StructSeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeTupleVariant = StructSeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeMap = SeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeStruct = StructSeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeStructVariant = StructSeqSerializer<'ser, 'sig, 'b, W>;

    serialize_basic!(serialize_bool, bool);
    serialize_basic!(serialize_i16, i16);
    serialize_basic!(serialize_i32, i32);
    serialize_basic!(serialize_i64, i64);

    serialize_basic!(serialize_u8, u8);
    serialize_basic!(serialize_u16, u16);
    serialize_basic!(serialize_u32, u32);
    serialize_basic!(serialize_u64, u64);

    serialize_basic!(serialize_f64, f64);

    fn serialize_i8(self, v: i8) -> Result<()> {
        // No i8 type in GVariant, let's pretend it's i16
        self.serialize_i16(v as i16)
    }

    fn serialize_f32(self, v: f32) -> Result<()> {
        // No f32 type in GVariant, let's pretend it's f64
        self.serialize_f64(v as f64)
    }

    fn serialize_char(self, v: char) -> Result<()> {
        // No char type in GVariant, let's pretend it's a string
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        if v.contains('\0') {
            return Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::Char('\0'),
                &"GVariant string type must not contain interior null bytes",
            ));
        }

        let c = self.0.sig_parser.next_char()?;
        if c == VARIANT_SIGNATURE_CHAR {
            self.0.value_sign = Some(signature_string!(v));

            // signature is serialized after the value in GVariant
            return Ok(());
        }

        // Strings in GVariant format require no alignment.

        self.0.sig_parser.skip_char()?;
        self.0
            .write_all(v.as_bytes())
            .map_err(|e| Error::InputOutput(e.into()))?;
        self.0
            .write_all(&b"\0"[..])
            .map_err(|e| Error::InputOutput(e.into()))?;

        Ok(())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<()> {
        let seq = self.serialize_seq(Some(v.len()))?;
        seq.ser
            .0
            .write(v)
            .map_err(|e| Error::InputOutput(e.into()))?;
        seq.end()
    }

    #[cfg(not(feature = "option-as-array"))]
    fn serialize_none(self) -> Result<()> {
        self.serialize_maybe::<()>(None)
    }

    #[cfg(feature = "option-as-array")]
    fn serialize_none(self) -> Result<()> {
        panic!("`option-as-array` and `gvariant` features are incompatible. Don't enable both.");
    }

    #[cfg(not(feature = "option-as-array"))]
    fn serialize_some<T>(self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.serialize_maybe(Some(value))
    }

    #[cfg(feature = "option-as-array")]
    fn serialize_some<T>(self, _value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        panic!("`option-as-array` and `gvariant` features are incompatible. Don't enable both.");
    }

    fn serialize_unit(self) -> Result<()> {
        self.0
            .write_all(&b"\0"[..])
            .map_err(|e| Error::InputOutput(e.into()))
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        variant: &'static str,
    ) -> Result<()> {
        if self.0.sig_parser.next_char()? == <&str>::SIGNATURE_CHAR {
            variant.serialize(self)
        } else {
            variant_index.serialize(self)
        }
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)?;

        Ok(())
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.0.prep_serialize_enum_variant(variant_index)?;

        value.serialize(self)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.0.sig_parser.skip_char()?;
        let element_signature = self.0.sig_parser.next_signature()?;
        let element_signature_len = element_signature.len();
        let element_alignment = alignment_for_signature(&element_signature, self.0.ctxt.format())?;

        let fixed_sized_child = crate::utils::is_fixed_sized_signature(&element_signature)?;
        let offsets = (!fixed_sized_child).then(FramingOffsets::new);

        let key_start = if self.0.sig_parser.next_char()? == DICT_ENTRY_SIG_START_CHAR {
            let key_signature = Signature::from_str_unchecked(&element_signature[1..2]);
            (!crate::utils::is_fixed_sized_signature(&key_signature)?).then_some(0)
        } else {
            None
        };
        self.0.add_padding(element_alignment)?;
        self.0.container_depths = self.0.container_depths.inc_array()?;

        let start = self.0.bytes_written;

        Ok(SeqSerializer {
            ser: self,
            start,
            element_alignment,
            element_signature_len,
            offsets,
            key_start,
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_struct("", len)
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_struct(name, len)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        self.0.prep_serialize_enum_variant(variant_index)?;

        StructSerializer::enum_variant(self).map(StructSeqSerializer::Struct)
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        self.serialize_seq(len)
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        if len == 0 {
            return StructSerializer::unit(self).map(StructSeqSerializer::Struct);
        }

        match self.0.sig_parser.next_char()? {
            VARIANT_SIGNATURE_CHAR => {
                StructSerializer::variant(self).map(StructSeqSerializer::Struct)
            }
            ARRAY_SIGNATURE_CHAR => self.serialize_seq(Some(len)).map(StructSeqSerializer::Seq),
            _ => StructSerializer::structure(self).map(StructSeqSerializer::Struct),
        }
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        self.0.prep_serialize_enum_variant(variant_index)?;

        StructSerializer::enum_variant(self).map(StructSeqSerializer::Struct)
    }

    fn is_human_readable(&self) -> bool {
        false
    }
}
