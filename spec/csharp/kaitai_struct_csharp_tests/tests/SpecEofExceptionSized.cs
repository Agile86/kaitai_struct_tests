// Autogenerated from KST: please remove this line if doing any edits by hand!

using NUnit.Framework;
using System.IO;

namespace Kaitai
{
    [TestFixture]
    public class SpecEofExceptionSized : CommonSpec
    {
        [Test]
        public void TestEofExceptionSized()
        {
            Assert.Throws<EndOfStreamException>(
                delegate
                {
                    EofExceptionSized.FromFile(SourceFile("term_strz.bin"));
                }
            );
        }
    }
}
