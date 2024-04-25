use super::{array_serializer::ArraySerializer, struct_serializer::StructSerializer};
use crate::{Error, Result};
use serde::{ser, ser::SerializeSeq, Serialize};
use std::{
    io::{Seek, Write},
    str,
};

#[doc(hidden)]
/// Allows us to serialize a struct as an ARRAY.
pub enum StructSeqSerializer<'ser, 'sig, 'b, W> {
    Struct(StructSerializer<'ser, 'sig, 'b, W>),
    Seq(ArraySerializer<'ser, 'sig, 'b, W>),
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
                }
            }

            fn end(self) -> Result<()> {
                match self {
                    StructSeqSerializer::Struct(ser) => ser.end_struct(),
                    StructSeqSerializer::Seq(ser) => ser.end_seq(),
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
                }
            }

            fn end(self) -> Result<()> {
                match self {
                    StructSeqSerializer::Struct(ser) => ser.end_struct(),
                    StructSeqSerializer::Seq(ser) => ser.end_seq(),
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
