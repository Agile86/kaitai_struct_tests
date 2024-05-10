// Autogenerated from KST: please remove this line if doing any edits by hand!

use std::fs;
extern crate kaitai;
use self::kaitai::*;
use rust::formats::switch_manual_int::*;

#[test]
fn test_switch_manual_int() -> KResult<()> {
    let bytes = fs::read("../../src/switch_opcodes.bin").unwrap();
    let _io = BytesReader::from(bytes);
    let r: OptRc<SwitchManualInt> = SwitchManualInt::read_into(&_io, None, None)?;

    assert_eq!(r.opcodes().len(), 4);
    assert_eq!(*r.opcodes()[0 as usize].code(), 83);
    assert_eq!(*Into::<OptRc<SwitchManualInt_Opcode_Strval>>::into(&*r.opcodes()[0 as usize].body().as_ref().unwrap()).value(), "foobar");
    assert_eq!(*r.opcodes()[1 as usize].code(), 73);
    assert_eq!(*Into::<OptRc<SwitchManualInt_Opcode_Intval>>::into(&*r.opcodes()[1 as usize].body().as_ref().unwrap()).value(), 66);
    assert_eq!(*r.opcodes()[2 as usize].code(), 73);
    assert_eq!(*Into::<OptRc<SwitchManualInt_Opcode_Intval>>::into(&*r.opcodes()[2 as usize].body().as_ref().unwrap()).value(), 55);
    assert_eq!(*r.opcodes()[3 as usize].code(), 83);
    assert_eq!(*Into::<OptRc<SwitchManualInt_Opcode_Strval>>::into(&*r.opcodes()[3 as usize].body().as_ref().unwrap()).value(), "");
    Ok(())
}
