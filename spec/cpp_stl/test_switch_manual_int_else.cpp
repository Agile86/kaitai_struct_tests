#include <boost/test/unit_test.hpp>

#include <switch_manual_int_else.h>

#include <iostream>
#include <fstream>
#include <vector>

BOOST_AUTO_TEST_CASE(test_switch_manual_int_else) {
    std::ifstream ifs("src/switch_opcodes2.bin", std::ifstream::binary);
    kaitai::kstream ks(&ifs);
    switch_manual_int_else_t* r = new switch_manual_int_else_t(&ks);

    BOOST_CHECK_EQUAL(r->opcodes()->size(), 4);

    BOOST_CHECK_EQUAL(r->opcodes()->at(0)->code(), 83);
    BOOST_CHECK_EQUAL((static_cast<switch_manual_int_else_t::opcode_t::strval_t*>(r->opcodes()->at(0)->body()))->value(), "foo");

    BOOST_CHECK_EQUAL(r->opcodes()->at(1)->code(), 88);
    BOOST_CHECK_EQUAL((static_cast<switch_manual_int_else_t::opcode_t::noneval_t*>(r->opcodes()->at(1)->body()))->filler(), 0x42);

    BOOST_CHECK_EQUAL(r->opcodes()->at(2)->code(), 89);
    BOOST_CHECK_EQUAL((static_cast<switch_manual_int_else_t::opcode_t::noneval_t*>(r->opcodes()->at(2)->body()))->filler(), 0xcafe);

    BOOST_CHECK_EQUAL(r->opcodes()->at(3)->code(), 73);
    BOOST_CHECK_EQUAL((static_cast<switch_manual_int_else_t::opcode_t::intval_t*>(r->opcodes()->at(3)->body()))->value(), 7);

    delete r;
}
