<?php
namespace Kaitai\Struct\Tests;

class StrEncodingsEscapingToSTest extends TestCase {
    public function testStrEncodingsEscapingToS() {
        $r = StrEncodingsEscapingToS::fromFile(self::SRC_DIR_PATH . '/str_encodings.bin');

        $this->assertUnknownEncoding("ASCII\\\\x", function () use ($r) {
            $r->str1();
        });
        $this->assertUnknownEncoding("UTF-8\\'x", function () use ($r) {
            $r->str2();
        });
        $this->assertUnknownEncoding("SJIS\\\"x", function () use ($r) {
            $r->str3();
        });
        $this->assertUnknownEncoding("IBM437\\nx", function () use ($r) {
            $r->str4();
        });
    }

    // FIXME: fix duplication of `StrEncodingsEscapingEncTest.php`
    private function assertUnknownEncoding(string $expectedEncoding, callable $fn) {
        try {
            $fn();
            $this->fail("A warning was expected, but none thrown");
        // `Warning` in PHP 8.3, `Notice` in PHP 7.1
        } catch (\PHPUnit\Framework\Error\Warning $e) {
            $this->assertSame('iconv(): Wrong encoding, conversion from "' . $expectedEncoding . '" to "utf-8" is not allowed', $e->getMessage());
        } catch (\PHPUnit\Framework\Error\Notice $e) {
            $this->assertSame('iconv(): Wrong charset, conversion from `' . $expectedEncoding . '\' to `utf-8\' is not allowed', $e->getMessage());
        }
    }
}
