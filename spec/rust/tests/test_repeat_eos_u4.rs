// Autogenerated from KST: please remove this line if doing any edits by hand!

#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(overflowing_literals)]
use std::fs;
extern crate kaitai;
use self::kaitai::*;
#[path = "../formats/mod.rs"] mod formats;
use formats::repeat_eos_u4::*;

#[test]
fn test_repeat_eos_u4() {
    let bytes = fs::read("../../src/repeat_eos_struct.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let res: KResult<OptRc<RepeatEosU4>> = RepeatEosU4::read_into(&_io, None, None);
    let r : OptRc<RepeatEosU4>;

    if let Err(err) = res {
        panic!("{:?}", err);
    } else {
        r = res.unwrap();
    }

    assert_eq!(*r.numbers(), vec![0, 66, 66, 2069]);
}