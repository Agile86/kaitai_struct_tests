//test_nav_parent_switch_cast.rs
#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(overflowing_literals)]
use std::fs;

extern crate kaitai;
use self::kaitai::*;
use rust::formats::nav_parent_switch_cast::*;

#[test]
fn test_nav_parent_switch_cast() {
    let reader = BytesReader::open("switch_integers.bin").unwrap();
    let r: kaitai::OptRc<NavParentSwitchCast> =
        NavParentSwitchCast::read_into(&reader, None, None).unwrap();

    let r_main = r.main();
    assert_eq!(*r_main.buf_type(), 1);
    assert_eq!(*r_main.flag(), 7);
    let x = r_main.buf().clone();
    if let Some(NavParentSwitchCast_Foo_Buf::NavParentSwitchCast_Foo_One(ref one)) = x {
        assert_eq!(*one.branch().flag().unwrap(), 7);
    } else {
        unreachable!()
    }
}
