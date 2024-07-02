// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::expr_io_ternary::*;

#[test]
fn test_expr_io_ternary() -> KResult<()> {
    let bytes = fs::read("../../src/if_struct.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let r: OptRc<ExprIoTernary> = ExprIoTernary::read_into(&_io, None, None)?;

    assert_eq!(*r.one_or_two_io_size1()?, 8);
    assert_eq!(*r.one_or_two_io_size2()?, 8);
    assert_eq!(*r.one_or_two_io_size_add_3()?, 11);
    Ok(())
}