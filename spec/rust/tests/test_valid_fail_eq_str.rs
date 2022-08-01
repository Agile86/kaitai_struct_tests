// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;

extern crate kaitai;
use self::kaitai::*;
mod formats;
use formats::valid_fail_eq_str::*;

#[test]
fn test_valid_fail_eq_str() {
    let bytes = fs::read("../../src/fixed_struct.bin").unwrap();
    let reader = BytesReader::new(&bytes);
    let mut r = ValidFailEqStr::default();

    if let Err(err) = r.read(&reader, None, KStructUnit::parent_stack()) {
        println!("expected err: {:?}, exception: ValidationNotEqualError(CalcStrType)", err);
    } else {
        panic!("no expected exception: ValidationNotEqualError(CalcStrType)");
    }
}