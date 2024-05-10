// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::expr_ops_parens::*;

#[test]
fn test_expr_ops_parens() -> KResult<()> {
    let bytes = fs::read("../../src/enum_negative.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let r: OptRc<ExprOpsParens> = ExprOpsParens::read_into(&_io, None, None)?;

    assert_eq!(*r.i_sum_to_str()?, "29");
    assert_eq!(*r.f_sum_to_int()?, 9);
    assert_eq!(*r.str_concat_len()?, 10);
    assert_eq!(*r.str_concat_rev()?, "9876543210");
    assert_eq!(*r.str_concat_substr_2_to_7()?, "23456");
    assert_eq!(*r.str_concat_to_i()?, 123456789);
    assert_eq!(*r.bool_eq()?, 0);
    assert_eq!(*r.bool_and()?, 0);
    assert_eq!(*r.bool_or()?, 1);
    Ok(())
}
