use serde::{ser::SerializeSeq, Serialize};
use std::io::{Seek, Write};

use crate::{framing_offsets::FramingOffsets, Error, Result};

use super::Serializer;

#[doc(hidden)]
pub struct ArraySerializer<'ser, 'sig, 'b, W> {
    pub(crate) ser: &'b mut Serializer<'ser, 'sig, W>,
    pub(super) start: usize,
    // size of element signature
    pub(super) element_signature_len: usize,
    // All offsets
    pub(super) offsets: Option<FramingOffsets>,
}

impl<'ser, 'sig, 'b, W> SerializeSeq for ArraySerializer<'ser, 'sig, 'b, W>
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

        if let Some(ref mut offsets) = self.offsets {
            let offset = self.ser.0.bytes_written - self.start;

            offsets.push(offset);
        }

        Ok(())
    }

    fn end(self) -> Result<()> {
        self.ser
            .0
            .sig_parser
            .skip_chars(self.element_signature_len)?;
        self.ser.0.container_depths = self.ser.0.container_depths.dec_array();

        let offsets = match self.offsets {
            Some(offsets) => offsets,
            None => return Ok(()),
        };
        let array_len = self.ser.0.bytes_written - self.start;
        if array_len == 0 {
            // Empty sequence
            return Ok(());
        }

        offsets.write_all(&mut self.ser.0, array_len)?;

        Ok(())
    }
}
