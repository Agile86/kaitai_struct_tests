// Autogenerated from KST: please remove this line if doing any edits by hand!

package io.kaitai.struct.spec;

import io.kaitai.struct.testformats.ImportsCircularA;
import org.testng.annotations.Test;
import static org.testng.Assert.*;
public class TestImportsCircularA extends CommonSpec {

    @Test
    public void testImportsCircularA() throws Exception {
        ImportsCircularA r = ImportsCircularA.fromFile(SRC_DIR + "fixed_struct.bin");

        assertIntEquals(r.code(), 80);
        assertIntEquals(r.two().initial(), 65);
        assertIntEquals(r.two().backRef().code(), 67);
        assertIntEquals(r.two().backRef().two().initial(), 75);
        assertNull(r.two().backRef().two().backRef());
    }
}
