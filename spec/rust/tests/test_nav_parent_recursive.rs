#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(overflowing_literals)]
use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::nav_parent_recursive::*;

#[test]
fn test_nav_parent_recursive() {
    let bytes = fs::read("../../src/enum_negative.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let res: KResult<OptRc<NavParentRecursive>> = NavParentRecursive::read_into(&_io, None, None);
    let r: OptRc<NavParentRecursive>;

    if let Err(err) = res {
        panic!("{:?}", err);
    } else {
        r = res.unwrap();
    }

    assert_eq!(*r.value(), 255);
    assert_eq!(*r.next().value(), 1);
    assert_eq!(*r.next().parent_value().unwrap(), 255);
    assert!(r.next().next().is_none());
}
