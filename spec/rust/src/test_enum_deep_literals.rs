// Autogenerated from KST: please remove this line if doing any edits by hand!

extern crate kaitai_struct;
extern crate kaitai_rust;

use kaitai_struct::KaitaiStruct;
use rust::EnumDeepLiterals;

#[test]
fn test_enum_deep_literals() {
    if let Ok(r) = EnumDeepLiterals::from_file("src/enum_0.bin") {

        assert_eq!(r.is_pet_1_ok, true);
        assert_eq!(r.is_pet_2_ok, true);
    }
}