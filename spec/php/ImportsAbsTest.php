<?php
// Autogenerated from KST: please remove this line if doing any edits by hand!

namespace Kaitai\Struct\Tests;

class ImportsAbsTest extends TestCase {
    public function testImportsAbs() {
        $r = ImportsAbs::fromFile(self::SRC_DIR_PATH . '/fixed_struct.bin');

        $this->assertSame(80, $r->len()->value());
        $this->assertSame(80, strlen($r->body()));
    }
}
