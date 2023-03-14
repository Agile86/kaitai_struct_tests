// Autogenerated from KST: please remove this line if doing any edits by hand!

#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(overflowing_literals)]
use std::fs;
extern crate kaitai;
use self::kaitai::*;
#[path = "../formats/mod.rs"] mod formats;
use formats::bytes_pad_term::*;

#[test]
fn test_bytes_pad_term() {
    let bytes = fs::read("../../src/str_pad_term.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let res: KResult<OptRc<BytesPadTerm>> = BytesPadTerm::read_into(&_io, None, None);
    let r : OptRc<BytesPadTerm>;

    if let Err(err) = res {
        panic!("{:?}", err);
    } else {
        r = res.unwrap();
    }

    assert_eq!(*r.str_pad(), vec![0x73u8, 0x74u8, 0x72u8, 0x31u8]);
    assert_eq!(*r.str_term(), vec![0x73u8, 0x74u8, 0x72u8, 0x32u8, 0x66u8, 0x6fu8, 0x6fu8]);
    assert_eq!(*r.str_term_and_pad(), vec![0x73u8, 0x74u8, 0x72u8, 0x2bu8, 0x2bu8, 0x2bu8, 0x33u8, 0x62u8, 0x61u8, 0x72u8, 0x2bu8, 0x2bu8, 0x2bu8]);
    assert_eq!(*r.str_term_include(), vec![0x73u8, 0x74u8, 0x72u8, 0x34u8, 0x62u8, 0x61u8, 0x7au8, 0x40u8]);
}