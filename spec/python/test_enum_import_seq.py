# Autogenerated from KST: please remove this line if doing any edits by hand!

import unittest
from enum_import_seq import EnumImportSeq
from enum_0 import Enum0
from enum_deep import EnumDeep

class TestEnumImportSeq(unittest.TestCase):
    def test_enum_import_seq(self):
        with EnumImportSeq.from_file('src/enum_0.bin') as r:

            self.assertEqual(r.pet_1, Enum0.Animal.cat)
            self.assertEqual(r.pet_2, EnumDeep.Container1.Container2.Animal.hare)
