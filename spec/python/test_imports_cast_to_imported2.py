# Autogenerated from KST: please remove this line if doing any edits by hand!

import unittest

from imports_cast_to_imported2 import ImportsCastToImported2

class TestImportsCastToImported2(unittest.TestCase):
    def test_imports_cast_to_imported2(self):
        with ImportsCastToImported2.from_file('src/process_xor_4.bin') as r:

            self.assertEqual(r.hw.one, 236)
            self.assertEqual(r.two.hw.one, 236)
