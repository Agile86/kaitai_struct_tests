// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#![allow(unused_imports)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]

use kaitai::*;
use std::{fs, path::PathBuf, convert::{TryFrom, TryInto}};
// extraAttrForIO(NamedIdentifier(buf), NoRepeat)

#[derive(Default, Debug, PartialEq)]
pub struct ProcessXorValue {
    pub key: u8,
    pub buf: Vec<u8>,
    pub raw_buf: Vec<u8>,
}
impl<'r, 's: 'r> KStruct<'r, 's> for ProcessXorValue {
    type Root = Self;
    type ParentStack = KStructUnit;

    fn read<S: KStream>(
        &mut self,
        _io: &'s S,
        _root: Option<&'r Self::Root>,
        _parent: Option<TypedStack<Self::ParentStack>>
    ) -> KResult<()> {
        self.key = _io.read_u1()?;
        // attrProcess(ProcessXor(Name(identifier(key))), RawIdentifier(NamedIdentifier(buf)), NamedIdentifier(buf), NoRepeat)
        Ok(())
    }
}
impl<'r, 's: 'r> ProcessXorValue {
    pub fn from_file(path: &str) -> Self {
        let bytes = fs::read(path).unwrap();
        let reader = BytesReader::new(&bytes);
        let mut obj = ProcessXorValue::default();

        if let Err(err) = obj.read(&reader, None, None) {
            panic!("error '{:?}' reading from file '{}'", err, path);
        }

        obj
    }

}