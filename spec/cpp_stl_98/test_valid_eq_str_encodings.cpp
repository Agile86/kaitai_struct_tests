// Autogenerated from KST: please remove this line if doing any edits by hand!

#include <boost/test/unit_test.hpp>
#include "valid_eq_str_encodings.h"
#include <iostream>
#include <fstream>
#include <vector>

BOOST_AUTO_TEST_CASE(test_valid_eq_str_encodings) {
    std::ifstream ifs("src/str_encodings.bin", std::ifstream::binary);
    kaitai::kstream ks(&ifs);
    valid_eq_str_encodings_t* r = new valid_eq_str_encodings_t(&ks);


    delete r;
}