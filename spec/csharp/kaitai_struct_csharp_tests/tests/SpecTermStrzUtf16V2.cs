// Autogenerated from KST: please remove this line if doing any edits by hand!

using NUnit.Framework;

namespace Kaitai
{
    [TestFixture]
    public class SpecTermStrzUtf16V2 : CommonSpec
    {
        [Test]
        public void TestTermStrzUtf16V2()
        {
            var r = TermStrzUtf16V2.FromFile(SourceFile("term_strz_utf16.bin"));

            Assert.AreEqual(r.S1, "a\u0200b");
            Assert.AreEqual(r.S2, "c\u0200d\0");
            Assert.AreEqual(r.S3, "e\u0200f");
        }
    }
}
