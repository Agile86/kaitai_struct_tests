#![allow(dead_code)]
use std::fs;

extern crate kaitai;
use self::kaitai::*;
mod formats;
use formats::process_coerce_bytes::*;

#[test]
fn test_process_coerce_bytes() {
    let bytes = fs::read("../../src/process_coerce_bytes.bin").unwrap();
    let reader = BytesReader::new(&bytes);
    let mut r = ProcessCoerceBytes::default();

    if let Err(err) = r.read(&reader, None, Some(KStructUnit::parent_stack())) {

        panic!("{:?}", err);
    }
    assert_eq!(0, *r.records()[0].flag());
    let buf : Vec<u8> = vec![0x41, 0x41, 0x41, 0x41];
    assert_eq!(buf, *r.records()[0].buf(&reader).unwrap());

    assert_eq!(1, *r.records()[1].flag());
    let buf : Vec<u8> = vec![0x42, 0x42, 0x42, 0x42];
    assert_eq!(buf, *r.records()[1].buf(&reader).unwrap());
}
