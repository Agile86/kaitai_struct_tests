// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::bits_unaligned_b64_le::*;

#[test]
fn test_bits_unaligned_b64_le() -> KResult<()> {
    let bytes = fs::read("../../src/process_xor_4.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let r: OptRc<BitsUnalignedB64Le> = BitsUnalignedB64Le::read_into(&_io, None, None)?;

    assert_eq!(*r.a(), false);
    assert_eq!(*r.b(), 1902324737369038326);
    assert_eq!(*r.c(), 71);
    Ok(())
}
