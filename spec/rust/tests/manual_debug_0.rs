#![allow(dead_code)]

extern crate kaitai;
mod formats;
use formats::debug_0::*;

#[test]
fn basic_parse() {
    let r = Debug0::default();
    assert_eq!(*r.one(), 0);
}