use serde::{ser, Serialize};
use std::io::{Seek, Write};

use crate::{utils::*, Error, Result, WriteBytes};

use super::Serializer;

#[doc(hidden)]
pub struct ArraySerializer<'ser, 'sig, 'b, W> {
    pub(super) ser: &'b mut Serializer<'ser, 'sig, W>,
    start: usize,
    // size of element signature
    element_signature_len: usize,
    // First element's padding
    first_padding: usize,
}

impl<'ser, 'sig, 'b, W> ArraySerializer<'ser, 'sig, 'b, W>
where
    W: Write + Seek,
{
    pub(super) fn new(
        ser: &'b mut Serializer<'ser, 'sig, W>,
        start: usize,
        element_signature_len: usize,
        first_padding: usize,
    ) -> Self {
        Self {
            ser,
            start,
            element_signature_len,
            first_padding,
        }
    }
}

impl<'ser, 'sig, 'b, W> ser::SerializeSeq for ArraySerializer<'ser, 'sig, 'b, W>
where
    W: Write + Seek,
{
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        // We want to keep parsing the same signature repeatedly for each element so we use a
        // disposable clone.
        let sig_parser = self.ser.0.sig_parser.clone();
        self.ser.0.sig_parser = sig_parser.clone();

        value.serialize(&mut *self.ser)?;
        self.ser.0.sig_parser = sig_parser;

        Ok(())
    }

    fn end(self) -> Result<()> {
        self.ser
            .0
            .sig_parser
            .skip_chars(self.element_signature_len)?;

        // Set size of array in bytes
        let array_len = self.ser.0.bytes_written - self.start;
        let len = usize_to_u32(array_len);
        let total_array_len = (array_len + self.first_padding + 4) as i64;
        self.ser
            .0
            .writer
            .seek(std::io::SeekFrom::Current(-total_array_len))
            .map_err(|e| Error::InputOutput(e.into()))?;
        self.ser
            .0
            .writer
            .write_u32(self.ser.0.ctxt.endian(), len)
            .map_err(|e| Error::InputOutput(e.into()))?;
        self.ser
            .0
            .writer
            .seek(std::io::SeekFrom::Current(total_array_len - 4))
            .map_err(|e| Error::InputOutput(e.into()))?;

        self.ser.0.container_depths = self.ser.0.container_depths.dec_array();

        Ok(())
    }
}
