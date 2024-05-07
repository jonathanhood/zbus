use serde::de::{self, DeserializeSeed, EnumAccess, MapAccess, SeqAccess, Visitor};
use static_assertions::assert_impl_all;

use std::{marker::PhantomData, str};

#[cfg(unix)]
use std::os::fd::AsFd;

use crate::{
    de::{DeserializerCommon, ValueParseStage},
    serialized::{Context, Format},
    signature_parser::SignatureParser,
    utils::*,
    Basic, Error, ObjectPath, Result, Signature, Value,
};

#[cfg(unix)]
use crate::Fd;

/// Our D-Bus deserialization implementation.
#[derive(Debug)]
pub(crate) struct Deserializer<'de, 'sig, 'f, F>(pub(crate) DeserializerCommon<'de, 'sig, 'f, F>);

assert_impl_all!(Deserializer<'_, '_, '_, ()>: Send, Sync, Unpin);

impl<'de, 'sig, 'f, F> Deserializer<'de, 'sig, 'f, F> {
    /// Create a Deserializer struct instance.
    ///
    /// On Windows, there is no `fds` argument.
    pub fn new<'r: 'de, S>(
        bytes: &'r [u8],
        #[cfg(unix)] fds: Option<&'f [F]>,
        signature: S,
        ctxt: Context,
    ) -> Result<Self>
    where
        S: TryInto<Signature<'sig>>,
        S::Error: Into<Error>,
    {
        assert_eq!(ctxt.format(), Format::DBus);

        let signature = signature.try_into().map_err(Into::into)?;
        let sig_parser = SignatureParser::new(signature);
        Ok(Self(DeserializerCommon {
            ctxt,
            sig_parser,
            bytes,
            #[cfg(unix)]
            fds,
            #[cfg(not(unix))]
            fds: PhantomData,
            pos: 0,
            container_depths: Default::default(),
            last_parsed_signature: None,
        }))
    }
}

macro_rules! deserialize_basic {
    ($method:ident $read_method:ident $visitor_method:ident($type:ty)) => {
        fn $method<V>(self, visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            if self.0.sig_parser.next_char()? == VARIANT_SIGNATURE_CHAR {
                let value: Value<'_> = serde::Deserialize::deserialize(&mut *self)?;
                return visitor.$visitor_method(value.try_into()?);
            } else {
                let v = self
                    .0
                    .ctxt
                    .endian()
                    .$read_method(self.0.next_const_size_slice::<$type>()?);

                visitor.$visitor_method(v)
            }
        }
    };
}

macro_rules! deserialize_as {
    ($method:ident => $as:ident) => {
        deserialize_as!($method() => $as());
    };
    ($method:ident($($in_arg:ident: $type:ty),*) => $as:ident($($as_arg:expr),*)) => {
        #[inline]
        fn $method<V>(self, $($in_arg: $type,)* visitor: V) -> Result<V::Value>
        where
            V: Visitor<'de>,
        {
            self.$as($($as_arg,)* visitor)
        }
    }
}

impl<'de, 'd, 'sig, 'f, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F> de::Deserializer<'de>
    for &'d mut Deserializer<'de, 'sig, 'f, F>
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let c = self.0.sig_parser.next_char()?;

        crate::de::deserialize_any::<Self, V>(self, c, visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let v = self
            .0
            .ctxt
            .endian()
            .read_u32(self.0.next_const_size_slice::<bool>()?);
        let b = match v {
            1 => true,
            0 => false,
            // As per D-Bus spec, only 0 and 1 values are allowed
            _ => {
                return Err(de::Error::invalid_value(
                    de::Unexpected::Unsigned(v as u64),
                    &"0 or 1",
                ))
            }
        };

        visitor.visit_bool(b)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i16(visitor)
    }

    deserialize_basic!(deserialize_i16 read_i16 visit_i16(i16));
    deserialize_basic!(deserialize_i64 read_i64 visit_i64(i64));
    deserialize_basic!(deserialize_u16 read_u16 visit_u16(u16));
    deserialize_basic!(deserialize_u32 read_u32 visit_u32(u32));
    deserialize_basic!(deserialize_u64 read_u64 visit_u64(u64));
    deserialize_basic!(deserialize_f64 read_f64 visit_f64(f64));

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let bytes = deserialize_ay(self)?;
        visitor.visit_byte_buf(bytes.into())
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let bytes = deserialize_ay(self)?;
        visitor.visit_borrowed_bytes(bytes)
    }

    deserialize_as!(deserialize_char => deserialize_str);
    deserialize_as!(deserialize_string => deserialize_str);
    deserialize_as!(deserialize_tuple(_l: usize) => deserialize_struct("", &[]));
    deserialize_as!(deserialize_tuple_struct(n: &'static str, _l: usize) => deserialize_struct(n, &[]));
    deserialize_as!(deserialize_struct(_n: &'static str, _f: &'static [&'static str]) => deserialize_seq());
    deserialize_as!(deserialize_map => deserialize_seq);
    deserialize_as!(deserialize_ignored_any => deserialize_any);

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let v: i32 = match self.0.sig_parser.next_char()? {
            #[cfg(unix)]
            Fd::SIGNATURE_CHAR => {
                self.0.sig_parser.skip_char()?;
                let alignment = u32::alignment(Format::DBus);
                self.0.parse_padding(alignment)?;
                let idx = self.0.ctxt.endian().read_u32(self.0.next_slice(alignment)?);
                self.0.get_fd(idx)?
            }
            VARIANT_SIGNATURE_CHAR => {
                let value: Value<'_> = serde::Deserialize::deserialize(&mut *self)?;
                value.try_into()?
            }
            _ => self
                .0
                .ctxt
                .endian()
                .read_i32(self.0.next_const_size_slice::<i32>()?),
        };

        visitor.visit_i32(v)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if self.0.sig_parser.next_char()? == VARIANT_SIGNATURE_CHAR {
            let value: Value<'_> = serde::Deserialize::deserialize(&mut *self)?;
            visitor.visit_u8(value.try_into()?)
        } else {
            // Endianness is irrelevant for single bytes.
            visitor.visit_u8(self.0.next_const_size_slice::<u8>().map(|bytes| bytes[0])?)
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let v = self
            .0
            .ctxt
            .endian()
            .read_f64(self.0.next_const_size_slice::<f64>()?);

        visitor.visit_f32(f64_to_f32(v))
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let next_char = self.0.sig_parser.next_char()?;

        if next_char == VARIANT_SIGNATURE_CHAR {
            let value: Value<'de> = serde::Deserialize::deserialize(&mut *self)?;

            match value {
                Value::Str(value) => return visitor.visit_str(value.as_str()),
                Value::ObjectPath(value) => return visitor.visit_str(value.as_str()),
                Value::Signature(value) => return visitor.visit_str(value.as_str()),
                _ => {
                    return Err(de::Error::invalid_type(
                        de::Unexpected::Str(&value.to_string()),
                        &"string, object path, or signature",
                    ))
                }
            }
        }

        let len = match self.0.sig_parser.next_char()? {
            Signature::SIGNATURE_CHAR => {
                let len_slice = self.0.next_slice(1)?;

                len_slice[0] as usize
            }
            <&str>::SIGNATURE_CHAR | ObjectPath::SIGNATURE_CHAR => {
                let alignment = u32::alignment(Format::DBus);
                self.0.parse_padding(alignment)?;
                let len_slice = self.0.next_slice(alignment)?;

                self.0.ctxt.endian().read_u32(len_slice) as usize
            }
            c => {
                let expected = format!(
                    "`{}`, `{}`, `{}` or `{}`",
                    <&str>::SIGNATURE_STR,
                    Signature::SIGNATURE_STR,
                    ObjectPath::SIGNATURE_STR,
                    VARIANT_SIGNATURE_CHAR,
                );
                return Err(de::Error::invalid_type(
                    de::Unexpected::Char(c),
                    &expected.as_str(),
                ));
            }
        };
        let slice = self.0.next_slice(len)?;
        if slice.contains(&0) {
            return Err(serde::de::Error::invalid_value(
                serde::de::Unexpected::Char('\0'),
                &"D-Bus string type must not contain interior null bytes",
            ));
        }
        self.0.pos += 1; // skip trailing null byte
        let s = str::from_utf8(slice).map_err(Error::Utf8)?;

        if next_char == Signature::SIGNATURE_CHAR {
            let signature = Signature::try_from(s)?.into_owned();
            self.0.last_parsed_signature = Some(signature);
        }

        self.0.sig_parser.skip_char()?;

        visitor.visit_borrowed_str(s)
    }

    fn deserialize_option<V>(self, #[allow(unused)] visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "option-as-array")]
        {
            let c = self.0.sig_parser.next_char()?;
            if c != ARRAY_SIGNATURE_CHAR {
                return Err(de::Error::invalid_type(
                    de::Unexpected::Char(c),
                    &ARRAY_SIGNATURE_STR,
                ));
            }
            self.0.sig_parser.skip_char()?;
            // This takes care of parsing all the padding and getting the byte length.
            let len = ArrayDeserializer::new(self)?.len;
            if len == 0 {
                self.0.sig_parser.parse_next_signature()?;

                visitor.visit_none()
            } else {
                visitor.visit_some(self)
            }
        }

        #[cfg(not(feature = "option-as-array"))]
        Err(de::Error::custom(
            "Can only decode Option<T> from D-Bus format if `option-as-array` feature is enabled",
        ))
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.0.sig_parser.next_char()? {
            VARIANT_SIGNATURE_CHAR => {
                let value_de = ValueDeserializer::new(self);
                visitor.visit_seq(value_de)
            }
            ARRAY_SIGNATURE_CHAR => {
                self.0.sig_parser.skip_char()?;
                let next_signature_char = self.0.sig_parser.next_char()?;
                let array_de = ArrayDeserializer::new(self)?;

                if next_signature_char == DICT_ENTRY_SIG_START_CHAR {
                    visitor.visit_map(ArrayMapDeserializer(array_de))
                } else {
                    visitor.visit_seq(ArraySeqDeserializer(array_de))
                }
            }
            STRUCT_SIG_START_CHAR => {
                let signature = self.0.sig_parser.next_signature()?;
                let alignment = alignment_for_signature(&signature, Format::DBus)?;
                self.0.parse_padding(alignment)?;

                self.0.sig_parser.skip_char()?;

                self.0.container_depths = self.0.container_depths.inc_structure()?;
                let v = visitor.visit_seq(StructureDeserializer { de: self });
                self.0.container_depths = self.0.container_depths.dec_structure();

                v
            }
            u8::SIGNATURE_CHAR => {
                // Empty struct: encoded as a `0u8`.
                let _: u8 = serde::Deserialize::deserialize(&mut *self)?;

                visitor.visit_seq(StructureDeserializer { de: self })
            }
            c => Err(de::Error::invalid_type(
                de::Unexpected::Char(c),
                &format!(
                    "`{VARIANT_SIGNATURE_CHAR}`, `{ARRAY_SIGNATURE_CHAR}` or `{STRUCT_SIG_START_CHAR}`",
                )
                .as_str(),
            )),
        }
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let signature = self.0.sig_parser.next_signature()?;
        let alignment = alignment_for_signature(&signature, self.0.ctxt.format())?;
        self.0.parse_padding(alignment)?;

        let non_unit = if self.0.sig_parser.next_char()? == STRUCT_SIG_START_CHAR {
            // This means we've a non-unit enum. Let's skip the `(`.
            self.0.sig_parser.skip_char()?;

            true
        } else {
            false
        };

        let v = visitor.visit_enum(crate::de::Enum {
            de: &mut *self,
            name,
            _phantom: PhantomData,
        })?;

        if non_unit {
            // For non-unit enum, we need to skip the closing paren.
            self.0.sig_parser.skip_char()?;
        }

        Ok(v)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if self.0.sig_parser.next_char()? == <&str>::SIGNATURE_CHAR {
            self.deserialize_str(visitor)
        } else {
            self.deserialize_u32(visitor)
        }
    }

    fn is_human_readable(&self) -> bool {
        false
    }
}

struct ArrayDeserializer<'d, 'de, 'sig, 'f, F> {
    de: &'d mut Deserializer<'de, 'sig, 'f, F>,
    len: usize,
    start: usize,
    // alignment of element
    element_alignment: usize,
    // where value signature starts
    element_signature_len: usize,
}

impl<'d, 'de, 'sig, 'f, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F>
    ArrayDeserializer<'d, 'de, 'sig, 'f, F>
{
    fn new(de: &'d mut Deserializer<'de, 'sig, 'f, F>) -> Result<Self> {
        de.0.parse_padding(ARRAY_ALIGNMENT_DBUS)?;
        de.0.container_depths = de.0.container_depths.inc_array()?;

        let len = de.0.ctxt.endian().read_u32(de.0.next_slice(4)?) as usize;
        let element_signature = de.0.sig_parser.next_signature()?;
        let element_alignment = alignment_for_signature(&element_signature, Format::DBus)?;
        let mut element_signature_len = element_signature.len();

        // D-Bus requires padding for the first element even when there is no first element
        // (i-e empty array) so we parse padding already.
        de.0.parse_padding(element_alignment)?;
        let start = de.0.pos;

        if de.0.sig_parser.next_char()? == DICT_ENTRY_SIG_START_CHAR {
            de.0.sig_parser.skip_char()?;
            element_signature_len -= 1;
        }

        Ok(Self {
            de,
            len,
            start,
            element_alignment,
            element_signature_len,
        })
    }

    fn next<T>(&mut self, seed: T, sig_parser: SignatureParser<'_>) -> Result<T::Value>
    where
        T: DeserializeSeed<'de>,
    {
        let ctxt = Context::new_dbus(
            self.de.0.ctxt.endian(),
            self.de.0.ctxt.position() + self.de.0.pos,
        );

        let mut de = Deserializer::<F>(DeserializerCommon {
            ctxt,
            sig_parser,
            bytes: subslice(self.de.0.bytes, self.de.0.pos..)?,
            fds: self.de.0.fds,
            pos: 0,
            container_depths: self.de.0.container_depths,
            last_parsed_signature: None,
        });
        let v = seed.deserialize(&mut de);
        self.de.0.pos += de.0.pos;
        // No need for retaking the container depths as the child can't be incomplete.

        if self.de.0.pos > self.start + self.len {
            return Err(serde::de::Error::invalid_length(
                self.len,
                &format!(">= {}", self.de.0.pos - self.start).as_str(),
            ));
        }

        v
    }

    fn next_element<T>(
        &mut self,
        seed: T,
        sig_parser: SignatureParser<'_>,
    ) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        if self.done() {
            self.de
                .0
                .sig_parser
                .skip_chars(self.element_signature_len)?;
            self.de.0.container_depths = self.de.0.container_depths.dec_array();

            return Ok(None);
        }

        self.de.0.parse_padding(self.element_alignment)?;

        self.next(seed, sig_parser).map(Some)
    }

    fn done(&self) -> bool {
        self.de.0.pos == self.start + self.len
    }
}

fn deserialize_ay<'de, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F>(
    de: &mut Deserializer<'de, '_, '_, F>,
) -> Result<&'de [u8]> {
    if de.0.sig_parser.next_signature()? != "ay" {
        return Err(de::Error::invalid_type(de::Unexpected::Seq, &"ay"));
    }

    de.0.sig_parser.skip_char()?;
    let ad = ArrayDeserializer::new(de)?;
    let len = ad.len;
    de.0.sig_parser.skip_char()?;
    de.0.next_slice(len)
}

struct ArraySeqDeserializer<'d, 'de, 'sig, 'f, F>(ArrayDeserializer<'d, 'de, 'sig, 'f, F>);

impl<'d, 'de, 'sig, 'f, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F> SeqAccess<'de>
    for ArraySeqDeserializer<'d, 'de, 'sig, 'f, F>
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        let sig_parser = self.0.de.0.sig_parser.clone();
        self.0.next_element(seed, sig_parser)
    }
}

struct ArrayMapDeserializer<'d, 'de, 'sig, 'f, F>(ArrayDeserializer<'d, 'de, 'sig, 'f, F>);

impl<'d, 'de, 'sig, 'f, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F> MapAccess<'de>
    for ArrayMapDeserializer<'d, 'de, 'sig, 'f, F>
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        let sig_parser = self.0.de.0.sig_parser.clone();
        self.0.next_element(seed, sig_parser)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        let mut sig_parser = self.0.de.0.sig_parser.clone();
        // Skip key signature (always 1 char)
        sig_parser.skip_char()?;
        self.0.next(seed, sig_parser)
    }
}

#[derive(Debug)]
struct StructureDeserializer<'d, 'de, 'sig, 'f, F> {
    de: &'d mut Deserializer<'de, 'sig, 'f, F>,
}

impl<'d, 'de, 'sig, 'f, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F> SeqAccess<'de>
    for StructureDeserializer<'d, 'de, 'sig, 'f, F>
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        let v = seed.deserialize(&mut *self.de).map(Some);

        if self.de.0.sig_parser.next_char()? == STRUCT_SIG_END_CHAR {
            // Last item in the struct
            self.de.0.sig_parser.skip_char()?;
        }

        v
    }
}

#[derive(Debug)]
struct ValueDeserializer<'d, 'de, 'sig, 'f, F> {
    de: &'d mut Deserializer<'de, 'sig, 'f, F>,
    stage: ValueParseStage,
}

impl<'d, 'de, 'sig, 'f, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F>
    ValueDeserializer<'d, 'de, 'sig, 'f, F>
{
    fn new(de: &'d mut Deserializer<'de, 'sig, 'f, F>) -> Self {
        ValueDeserializer::<F> {
            de,
            stage: ValueParseStage::Signature,
        }
    }
}

impl<'d, 'de, 'sig, 'f, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F> SeqAccess<'de>
    for ValueDeserializer<'d, 'de, 'sig, 'f, F>
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        match self.stage {
            ValueParseStage::Signature => {
                self.stage = ValueParseStage::Value;

                let signature = signature_string!("g");
                let sig_parser = SignatureParser::new(signature);

                let context = Context::new_dbus(
                    self.de.0.ctxt.endian(),
                    self.de.0.ctxt.position() + self.de.0.pos,
                );

                let mut de = Deserializer::<F>(DeserializerCommon {
                    ctxt: context,
                    sig_parser,
                    bytes: subslice(self.de.0.bytes, self.de.0.pos..)?,
                    fds: self.de.0.fds,
                    pos: 0,
                    container_depths: self.de.0.container_depths,
                    last_parsed_signature: None,
                });

                let result = seed.deserialize(&mut de);
                self.de.0.pos += de.0.pos;
                self.de.0.last_parsed_signature = de.0.last_parsed_signature;

                result.map(Some)
            }
            ValueParseStage::Value => {
                self.stage = ValueParseStage::Done;

                if self.de.0.last_parsed_signature.is_none() {
                    return Err(serde::de::Error::custom(
                        "ValueDeserializer: Signature not parsed",
                    ));
                }

                let signature = self.de.0.last_parsed_signature.take().unwrap();
                let sig_parser = SignatureParser::new(signature);

                let ctxt = Context::new(
                    Format::DBus,
                    self.de.0.ctxt.endian(),
                    self.de.0.ctxt.position() + self.de.0.pos,
                );

                let mut de = Deserializer::<F>(DeserializerCommon {
                    ctxt,
                    sig_parser,
                    bytes: subslice(self.de.0.bytes, self.de.0.pos..)?,
                    fds: self.de.0.fds,
                    pos: 0,
                    container_depths: self.de.0.container_depths.inc_variant()?,
                    last_parsed_signature: None,
                });

                let v = seed.deserialize(&mut de).map(Some);
                self.de.0.pos += de.0.pos;
                self.de.0.sig_parser.skip_char()?;

                v
            }
            ValueParseStage::Done => Ok(None),
        }
    }
}

impl<'de, 'd, 'sig, 'f, #[cfg(unix)] F: AsFd, #[cfg(not(unix))] F> EnumAccess<'de>
    for crate::de::Enum<&'d mut Deserializer<'de, 'sig, 'f, F>, F>
{
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.de).map(|v| (v, self))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{serialized::Data, OwnedObjectPath};
    use endi::LE;

    #[test]
    pub fn deserialize_variant_u8() {
        let bytes = [
            0x01, 'y' as u8, 0,    // Signature
            0x08, // Value
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: u8 = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, 0x08);
    }

    #[test]
    pub fn deserialize_variant_u16() {
        let bytes = [
            0x01, 'q' as u8, 0, // Signature
            0, // Pad to a 2-byte boundary
            0x0D, 0xF0, // Value
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: u16 = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, 0xF00D);
    }

    #[test]
    pub fn deserialize_variant_u32() {
        let bytes = [
            0x01, 'u' as u8, 0, // Signature
            0, // Pad to a 4-byte boundary
            0x0D, 0xF0, 0x37, 0x13, // Value
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: u32 = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, 0x1337F00D);
    }

    #[test]
    pub fn deserialize_variant_u64() {
        let bytes = [
            0x01, 't' as u8, 0, // Signature
            0, 0, 0, 0, 0, // Pad to a 8-byte boundary
            0x0D, 0xF0, 0x37, 0x13, 0xEF, 0xBE, 0xBE, 0xBA, // Value
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: u64 = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, 0xBABEBEEF1337F00D);
    }

    #[test]
    pub fn deserialize_variant_i16() {
        let bytes = [
            0x01, 'n' as u8, 0, // Signature
            0, // Pad to a 2-byte boundary
            0x37, 0x13, // Value
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: i16 = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, 0x1337);
    }

    #[test]
    pub fn deserialize_variant_i32() {
        let bytes = [
            0x01, 'i' as u8, 0, // Signature
            0, // Pad to a 4-byte boundary
            0xBE, 0xBA, 0x37, 0x13, // Value
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: i32 = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, 0x1337BABE);
    }

    #[test]
    pub fn deserialize_variant_i64() {
        let bytes = [
            0x01, 'x' as u8, 0, // Signature
            0, 0, 0, 0, 0, // Pad to a 8-byte boundary
            0xCE, 0xFA, 0x0D, 0xF0, 0xBE, 0xBA, 0x37, 0x13, // Value
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: i64 = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, 0x1337BABEF00DFACE);
    }

    #[test]
    pub fn deserialize_variant_double() {
        let bytes = [
            0x01, 'd' as u8, 0, // Signature
            0, 0, 0, 0, 0, // Pad to a 8-byte boundary
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40, // Value
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: f64 = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, 3.0f64);
    }

    #[test]
    pub fn deserialize_variant_string() {
        let bytes = [
            0x01, 's' as u8, 0, // Signature
            0, // Pad to a 4-byte boundary
            0x05, 0x00, 0x00, 0x00, // Length
            b'h', b'e', b'l', b'l', b'o', // Value
            0,    // Null terminator
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: String = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v, "hello");
    }

    #[test]
    pub fn deserialize_variant_object_path() {
        let bytes = [
            0x01, 'o' as u8, 0, // Signature
            0, // Pad to a 4-byte boundary
            0x04, 0x00, 0x00, 0x00, // Length
            b'/', b'o', b'r', b'g', // Object Path
            0,    // Null terminator
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: OwnedObjectPath = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v.as_str(), "/org");
    }

    #[test]
    pub fn deserialize_variant_signature() {
        let bytes = [
            0x01, 'g' as u8, 0, // Signature
            0x04, b'a', b'(', b'v', b')', // Signature
            0,    // Null terminator
        ];

        let context = Context::new_dbus(LE, 0);
        let data = Data::new(&bytes, context);

        let v: Signature<'_> = data.deserialize_for_signature("v").unwrap().0;
        assert_eq!(v.as_str(), "a(v)");
    }
}
