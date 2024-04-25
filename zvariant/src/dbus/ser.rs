mod array_serializer;
mod dict_serializer;
mod struct_seq_serializer;
mod struct_serializer;

use serde::{ser, ser::SerializeSeq, Serialize};
use static_assertions::assert_impl_all;
use std::{
    io::{Seek, Write},
    str,
};

use crate::{
    serialized::{Context, Format},
    signature_parser::SignatureParser,
    utils::*,
    Basic, Error, ObjectPath, Result, Signature, WriteBytes,
};

use self::{
    array_serializer::ArraySerializer, dict_serializer::DictSerializer,
    struct_seq_serializer::StructSeqSerializer, struct_serializer::StructSerializer,
};

#[cfg(unix)]
use crate::Fd;

/// Our D-Bus serialization implementation.
pub(crate) struct Serializer<'ser, 'sig, W>(pub(crate) crate::SerializerCommon<'ser, 'sig, W>);

assert_impl_all!(Serializer<'_, '_, i32>: Send, Sync, Unpin);

impl<'ser, 'sig, W> Serializer<'ser, 'sig, W>
where
    W: Write + Seek,
{
    /// Create a D-Bus Serializer struct instance.
    ///
    /// On Windows, there is no `fds` argument.
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
        assert_eq!(ctxt.format(), Format::DBus);

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
}

macro_rules! serialize_basic {
    ($method:ident($type:ty) $write_method:ident) => {
        serialize_basic!($method($type) $write_method($type));
    };
    ($method:ident($type:ty) $write_method:ident($as:ty)) => {
        fn $method(self, v: $type) -> Result<()> {
            self.0.prep_serialize_basic::<$type>()?;
            self.0.$write_method(self.0.ctxt.endian(), v as $as).map_err(|e| Error::InputOutput(e.into()))
        }
    };
}

impl<'ser, 'sig, 'b, W> ser::Serializer for &'b mut Serializer<'ser, 'sig, W>
where
    W: Write + Seek,
{
    type Ok = ();
    type Error = Error;

    type SerializeSeq = ArraySerializer<'ser, 'sig, 'b, W>;
    type SerializeTuple = StructSeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeTupleStruct = StructSeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeTupleVariant = StructSeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeMap = DictSerializer<'ser, 'sig, 'b, W>;
    type SerializeStruct = StructSeqSerializer<'ser, 'sig, 'b, W>;
    type SerializeStructVariant = StructSeqSerializer<'ser, 'sig, 'b, W>;

    serialize_basic!(serialize_bool(bool) write_u32(u32));
    // No i8 type in D-Bus/GVariant, let's pretend it's i16
    serialize_basic!(serialize_i8(i8) write_i16(i16));
    serialize_basic!(serialize_i16(i16) write_i16);
    serialize_basic!(serialize_i64(i64) write_i64);

    fn serialize_i32(self, v: i32) -> Result<()> {
        match self.0.sig_parser.next_char()? {
            #[cfg(unix)]
            Fd::SIGNATURE_CHAR => {
                self.0.sig_parser.skip_char()?;
                self.0.add_padding(u32::alignment(Format::DBus))?;
                let idx = self.0.add_fd(v)?;
                self.0
                    .write_u32(self.0.ctxt.endian(), idx)
                    .map_err(|e| Error::InputOutput(e.into()))
            }
            _ => {
                self.0.prep_serialize_basic::<i32>()?;
                self.0
                    .write_i32(self.0.ctxt.endian(), v)
                    .map_err(|e| Error::InputOutput(e.into()))
            }
        }
    }

    fn serialize_u8(self, v: u8) -> Result<()> {
        self.0.prep_serialize_basic::<u8>()?;
        // Endianness is irrelevant for single bytes.
        self.0
            .write_u8(self.0.ctxt.endian(), v)
            .map_err(|e| Error::InputOutput(e.into()))
    }

    serialize_basic!(serialize_u16(u16) write_u16);
    serialize_basic!(serialize_u32(u32) write_u32);
    serialize_basic!(serialize_u64(u64) write_u64);
    // No f32 type in D-Bus/GVariant, let's pretend it's f64
    serialize_basic!(serialize_f32(f32) write_f64(f64));
    serialize_basic!(serialize_f64(f64) write_f64);

    fn serialize_char(self, v: char) -> Result<()> {
        // No char type in D-Bus, let's pretend it's a string
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        if v.contains('\0') {
            return Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::Char('\0'),
                &"D-Bus string type must not contain interior null bytes",
            ));
        }
        let c = self.0.sig_parser.next_char()?;
        if c == VARIANT_SIGNATURE_CHAR {
            self.0.value_sign = Some(signature_string!(v));
        }

        match c {
            ObjectPath::SIGNATURE_CHAR | <&str>::SIGNATURE_CHAR => {
                self.0.add_padding(<&str>::alignment(Format::DBus))?;
                self.0
                    .write_u32(self.0.ctxt.endian(), usize_to_u32(v.len()))
                    .map_err(|e| Error::InputOutput(e.into()))?;
            }
            Signature::SIGNATURE_CHAR | VARIANT_SIGNATURE_CHAR => {
                self.0
                    .write_u8(self.0.ctxt.endian(), usize_to_u8(v.len()))
                    .map_err(|e| Error::InputOutput(e.into()))?;
            }
            _ => {
                let expected = format!(
                    "`{}`, `{}`, `{}` or `{}`",
                    <&str>::SIGNATURE_STR,
                    Signature::SIGNATURE_STR,
                    ObjectPath::SIGNATURE_STR,
                    VARIANT_SIGNATURE_CHAR,
                );
                return Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Char(c),
                    &expected.as_str(),
                ));
            }
        }

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

    fn serialize_none(self) -> Result<()> {
        #[cfg(feature = "option-as-array")]
        {
            let seq = self.serialize_seq(Some(0))?;
            seq.end()
        }

        #[cfg(not(feature = "option-as-array"))]
        unreachable!(
            "Can only encode Option<T> in D-Bus format if `option-as-array` feature is enabled",
        );
    }

    fn serialize_some<T>(self, #[allow(unused)] value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        #[cfg(feature = "option-as-array")]
        {
            let mut seq = self.serialize_seq(Some(1))?;
            seq.serialize_element(value)?;
            seq.end()
        }

        #[cfg(not(feature = "option-as-array"))]
        unreachable!(
            "Can only encode Option<T> in D-Bus format if `option-as-array` feature is enabled",
        );
    }

    fn serialize_unit(self) -> Result<()> {
        Ok(())
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
        value.serialize(&mut *self)?;
        // Skip the `)`.
        self.0.sig_parser.skip_char()?;

        Ok(())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        self.0.sig_parser.skip_char()?;
        self.0.add_padding(ARRAY_ALIGNMENT_DBUS)?;
        // Length in bytes (unfortunately not the same as len passed to us here) which we
        // initially set to 0.
        self.0
            .write_u32(self.0.ctxt.endian(), 0_u32)
            .map_err(|e| Error::InputOutput(e.into()))?;

        let element_signature = self.0.sig_parser.next_signature()?;
        let element_signature_len = element_signature.len();
        let element_alignment = alignment_for_signature(&element_signature, self.0.ctxt.format())?;

        // D-Bus expects us to add padding for the first element even when there is no first
        // element (i-e empty array) so we add padding already.
        let first_padding = self.0.add_padding(element_alignment)?;
        let start = self.0.bytes_written;
        self.0.container_depths = self.0.container_depths.inc_array()?;

        Ok(ArraySerializer::new(
            self,
            start,
            element_signature_len,
            first_padding,
        ))
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

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        self.0.sig_parser.skip_char()?;
        self.0.add_padding(ARRAY_ALIGNMENT_DBUS)?;
        // Length in bytes (unfortunately not the same as len passed to us here) which we
        // initially set to 0.
        self.0
            .write_u32(self.0.ctxt.endian(), 0_u32)
            .map_err(|e| Error::InputOutput(e.into()))?;

        let element_signature = self.0.sig_parser.next_signature()?;
        let element_signature_len = element_signature.len();
        let element_alignment = alignment_for_signature(&element_signature, self.0.ctxt.format())?;

        // D-Bus expects us to add padding for the first element even when there is no first
        // element (i-e empty array) so we add padding already.
        let first_padding = self.0.add_padding(element_alignment)?;
        let start = self.0.bytes_written;
        self.0.container_depths = self.0.container_depths.inc_array()?;

        Ok(DictSerializer::new(
            self,
            start,
            element_signature_len,
            element_alignment,
            first_padding,
        ))
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        if len == 0 {
            return StructSerializer::unit(self).map(StructSeqSerializer::Struct);
        }

        match self.0.sig_parser.next_char()? {
            VARIANT_SIGNATURE_CHAR => {
                StructSerializer::variant(self).map(StructSeqSerializer::Struct)
            }
            ARRAY_SIGNATURE_CHAR => {
                // Peek ahead to decide if this an array of elements
                // or an array of k/v pair DictEntries
                let mut sig_parser = self.0.sig_parser.clone();
                sig_parser.skip_char()?;
                let array_type = sig_parser.next_char()?;

                if array_type == DICT_ENTRY_SIG_START_CHAR {
                    self.serialize_map(Some(len)).map(StructSeqSerializer::Dict)
                } else {
                    self.serialize_seq(Some(len)).map(StructSeqSerializer::Seq)
                }
            }
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
