// Autogenerated from KST: please remove this line if doing any edits by hand!

extern crate kaitai_struct;
extern crate rust;

use kaitai_struct::KaitaiStruct;
use rust::ZlibSurrounded;

#[test]
fn test_zlib_surrounded() {
    if let Ok(r) = ZlibSurrounded::from_file("src/zlib_surrounded.bin") {

        assert_eq!(r.zlib.num, -1);
    }
}