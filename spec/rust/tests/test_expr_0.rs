// Autogenerated from KST: please remove this line if doing any edits by hand!

#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(overflowing_literals)]
use std::fs;
extern crate kaitai;
use self::kaitai::*;
#[path = "../formats/mod.rs"] mod formats;
use formats::expr_0::*;

#[test]
fn test_expr_0() {
    let bytes = fs::read("../../src/str_encodings.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let res: KResult<OptRc<Expr0>> = Expr0::read_into(&_io, None, None);
    let r : OptRc<Expr0>;

    if let Err(err) = res {
        panic!("{:?}", err);
    } else {
        r = res.unwrap();
    }

    assert_eq!(*r.must_be_f7().expect("error reading"), 247);
    assert_eq!(*r.must_be_abc123().expect("error reading"), "abc123");
}