package io.kaitai.struct.spec;

import io.kaitai.struct.testformats.StrLiterals2;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class TestStrLiterals2 extends CommonSpec {
    @Test
    public void testStrLiterals2() throws Exception {
        StrLiterals2 r = StrLiterals2.fromFile(SRC_DIR + "fixed_struct.bin");

        assertEquals(r.dollar1(), "$foo");
        assertEquals(r.dollar2(), "${foo}");
        assertEquals(r.hash(), "#{foo}");
        assertEquals(r.atSign(), "@foo");
    }
}