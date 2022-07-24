// Autogenerated from KST: please remove this line if doing any edits by hand!

extern crate kaitai_struct;
extern crate kaitai_rust;

use kaitai_struct::KaitaiStruct;
use rust::CastToImported;

#[test]
fn test_cast_to_imported() {
    if let Ok(r) = CastToImported::from_file("src/fixed_struct.bin") {

        assert_eq!(r.one.one, 80);
        assert_eq!(r.one_casted.one, 80);
    }
}