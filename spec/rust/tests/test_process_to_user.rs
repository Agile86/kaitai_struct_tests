// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::process_to_user::*;

#[test]
fn test_process_to_user() -> KResult<()> {
    let bytes = fs::read("../../src/process_rotate.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let r: OptRc<ProcessToUser> = ProcessToUser::read_into(&_io, None, None)?;

    assert_eq!(*r.buf1().str(), "Hello");
    Ok(())
}
