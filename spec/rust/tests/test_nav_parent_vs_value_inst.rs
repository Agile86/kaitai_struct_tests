// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::nav_parent_vs_value_inst::*;

#[test]
fn test_nav_parent_vs_value_inst() -> KResult<()> {
    let bytes = fs::read("../../src/term_strz.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let r: OptRc<NavParentVsValueInst> = NavParentVsValueInst::read_into(&_io, None, None)?;

    assert_eq!(*r.s1(), "foo");
    Ok(())
}
