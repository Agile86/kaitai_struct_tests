// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::process_repeat_usertype::*;

#[test]
fn test_process_repeat_usertype() -> KResult<()> {
    let bytes = fs::read("../../src/process_xor_4.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let r: OptRc<ProcessRepeatUsertype> = ProcessRepeatUsertype::read_into(&_io, None, None)?;

    assert_eq!(*r.blocks()[0 as usize].a(), -1975704206);
    assert_eq!(*r.blocks()[0 as usize].b(), 20);
    assert_eq!(*r.blocks()[1 as usize].a(), 279597642);
    assert_eq!(*r.blocks()[1 as usize].b(), 68);
    Ok(())
}
