<?php
// Autogenerated from KST: please remove this line if doing any edits by hand!

namespace Kaitai\Struct\Tests;

class ImportsCastToImportedTest extends TestCase {
    public function testImportsCastToImported() {
        $r = ImportsCastToImported::fromFile(self::SRC_DIR_PATH . '/process_xor_4.bin');

        $this->assertSame(236, $r->hw()->one());
        $this->assertSame(236, $r->two()->hwOne());
    }
}
