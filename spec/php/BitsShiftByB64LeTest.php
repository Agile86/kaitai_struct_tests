<?php
// Autogenerated from KST: please remove this line if doing any edits by hand!

namespace Kaitai\Struct\Tests;

class BitsShiftByB64LeTest extends TestCase {
    public function testBitsShiftByB64Le() {
        $r = BitsShiftByB64Le::fromFile(self::SRC_DIR_PATH . '/bits_shift_by_b64_le.bin');

        $this->assertSame(-1, $r->a());
        $this->assertSame(0, $r->b());
    }
}