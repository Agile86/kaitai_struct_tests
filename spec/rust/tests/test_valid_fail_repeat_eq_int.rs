// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::valid_fail_repeat_eq_int::*;

#[test]
fn test_valid_fail_repeat_eq_int() -> KResult<()> {
    let bytes = fs::read("../../src/bcd_user_type_be.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let res: KResult<OptRc<ValidFailRepeatEqInt>> = ValidFailRepeatEqInt::read_into(&_io, None, None);
    let err = res.expect_err("expected Err representing ValidationNotEqualError(IntMultiType(false,Width4,None)), but got Ok");
    assert!(
        matches!(err, KError::ValidationFailed(ValidationFailedError { kind: ValidationKind::NotEqual, .. })),
        "expected: {}\n but got: {:?}",
        "KError::ValidationFailed(ValidationFailedError { kind: ValidationKind::NotEqual, .. })", err
    );
    Ok(())
}