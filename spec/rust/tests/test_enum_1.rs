// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;

extern crate kaitai;
use self::kaitai::*;
mod formats;
use formats::enum_1::*;

#[test]
fn test_enum_1() {
    let bytes = fs::read("../../src/enum_0.bin").unwrap();
    let reader = BytesReader::new(&bytes);
    let mut r = Enum1::default();

    if let Err(err) = r.read(&reader, None, Some(KStructUnit::parent_stack())) {

        panic!("{:?}", err);
    }
    assert_eq!(r.main().submain().pet_1(), &Enum1_MainObj_Animal::Cat);
    assert_eq!(r.main().submain().pet_2(), &Enum1_MainObj_Animal::Chicken);
}
