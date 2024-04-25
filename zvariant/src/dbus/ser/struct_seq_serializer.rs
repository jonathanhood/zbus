use super::{
    array_serializer::ArraySerializer, dict_serializer::DictSerializer,
    struct_serializer::StructSerializer,
};
use crate::{Error, Result};
use serde::{
    ser,
    ser::{SerializeMap, SerializeSeq},
    Serialize,
};
use std::{
    io::{Seek, Write},
    str,
};

#[doc(hidden)]
/// Allows us to serialize a struct as an ARRAY.
pub enum StructSeqSerializer<'ser, 'sig, 'b, W> {
    Struct(StructSerializer<'ser, 'sig, 'b, W>),
    Seq(ArraySerializer<'ser, 'sig, 'b, W>),
    Dict(DictSerializer<'ser, 'sig, 'b, W>),
}

macro_rules! serialize_struct_anon_fields {
    ($trait:ident $method:ident) => {
        impl<'ser, 'sig, 'b, W> ser::$trait for StructSeqSerializer<'ser, 'sig, 'b, W>
        where
            W: Write + Seek,
        {
            type Ok = ();
            type Error = Error;

            fn $method<T>(&mut self, value: &T) -> Result<()>
            where
                T: ?Sized + Serialize,
            {
                match self {
                    StructSeqSerializer::Struct(ser) => ser.$method(value),
                    StructSeqSerializer::Seq(ser) => ser.serialize_element(value),
                    StructSeqSerializer::Dict(_ser) => {
                        unreachable!("Dictionaries don't support anonymous fields")
                    }
                }
            }

            fn end(self) -> Result<()> {
                match self {
                    StructSeqSerializer::Struct(ser) => ser.end_struct(),
                    StructSeqSerializer::Seq(ser) => ser.end(),
                    StructSeqSerializer::Dict(_ser) => {
                        unreachable!("Dictionaries don't support anonymous fields")
                    }
                }
            }
        }
    };
}

macro_rules! serialize_struct_named_fields {
    ($trait:ident) => {
        impl<'ser, 'sig, 'b, W> ser::$trait for StructSeqSerializer<'ser, 'sig, 'b, W>
        where
            W: Write + Seek,
        {
            type Ok = ();
            type Error = Error;

            fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
            where
                T: ?Sized + Serialize,
            {
                match self {
                    StructSeqSerializer::Struct(ser) => ser.serialize_field(key, value),
                    StructSeqSerializer::Seq(ser) => ser.serialize_element(value),
                    StructSeqSerializer::Dict(ser) => {
                        ser.serialize_key(key)?;
                        ser.serialize_value(value)
                    }
                }
            }

            fn end(self) -> Result<()> {
                match self {
                    StructSeqSerializer::Struct(ser) => ser.end_struct(),
                    StructSeqSerializer::Seq(ser) => ser.end(),
                    StructSeqSerializer::Dict(ser) => ser.end(),
                }
            }
        }
    };
}

serialize_struct_anon_fields!(SerializeTuple serialize_element);
serialize_struct_anon_fields!(SerializeTupleStruct serialize_field);
serialize_struct_anon_fields!(SerializeTupleVariant serialize_field);
serialize_struct_named_fields!(SerializeStruct);
serialize_struct_named_fields!(SerializeStructVariant);
