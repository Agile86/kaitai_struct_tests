<?php
// Autogenerated from KST: please remove this line if doing any edits by hand!

namespace Kaitai\Struct\Tests;

class ValidFailInstTest extends TestCase {
    public function testValidFailInst() {
        $this->expectException(\Kaitai\Struct\Error\ValidationNotEqualError::class);
        $r = ValidFailInst::fromFile(self::SRC_DIR_PATH . '/fixed_struct.bin');
    }
}
