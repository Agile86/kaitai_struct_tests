// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::enum_0::*;

#[test]
fn test_enum_0() -> KResult<()> {
    let bytes = fs::read("../../src/enum_0.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let r: OptRc<Enum0> = Enum0::read_into(&_io, None, None)?;

    assert_eq!(*r.pet_1(), Enum0_Animal::Cat);
    assert_eq!(*r.pet_2(), Enum0_Animal::Chicken);
    Ok(())
}
