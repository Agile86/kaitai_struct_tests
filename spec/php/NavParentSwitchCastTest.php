<?php
// Autogenerated from KST: please remove this line if doing any edits by hand!

namespace Kaitai\Struct\Tests;

class NavParentSwitchCastTest extends TestCase {
    public function testNavParentSwitchCast() {
        $r = NavParentSwitchCast::fromFile(self::SRC_DIR_PATH . '/switch_integers.bin');

        $this->assertSame(1, $r->foo()->bufType());
        $this->assertSame(7, $r->foo()->flag());
        $this->assertSame(7, $r->foo()->buf()->bar()->flag());
    }
}
