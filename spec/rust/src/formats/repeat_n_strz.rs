// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]

use kaitai::*;
use std::{fs, path::PathBuf, convert::{TryFrom, TryInto}};

#[derive(Default, Debug, PartialEq)]
pub struct RepeatNStrz {
    pub qty: u32,
    pub lines: Vec<String>,
}
impl<'r, 's: 'r> KStruct<'r, 's> for RepeatNStrz {
    type Root = Self;
    type ParentStack = KStructUnit;

    fn read<S: KStream>(
        &mut self,
        _io: &'s S,
        _root: Option<&'r Self::Root>,
        _parent: Option<TypedStack<Self::ParentStack>>
    ) -> KResult<()> {
        self.qty = _io.read_u4le()?;
        self.lines = Vec::new();
        {
            // condRepeatExprHeader(NamedIdentifier(lines), _io, StrFromBytesType(BytesTerminatedType(0,false,true,true,None),UTF-8), Name(identifier(qty)))
            // handleAssignmentRepeatExpr(NamedIdentifier(lines), decode_string(_io.read_bytes_term(0, false, true, true)?, "UTF-8")?)
        }
        Ok(())
    }
}
impl<'r, 's: 'r> RepeatNStrz {
    pub fn from_file(path: &str) -> Self {
        let bytes = fs::read(path).unwrap();
        let reader = BytesReader::new(&bytes);
        let mut obj = RepeatNStrz::default();

        if let Err(err) = obj.read(&reader, None, None) {
            panic!("error '{:?}' reading from file '{}'", err, path);
        }

        obj
    }

}