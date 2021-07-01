// Autogenerated from KST: please remove this line if doing any edits by hand!

var assert = require('assert');
var testHelper = require('testHelper');

testHelper('ExprStrEncodings', 'src/str_encodings.bin', function(r, ExprStrEncodings) {

  assert.strictEqual(r.str1Eq, true);
  assert.strictEqual(r.str2Eq, true);
  assert.strictEqual(r.str3Eq, true);
  assert.strictEqual(r.str3EqStr2, true);
  assert.strictEqual(r.str4Eq, true);
  assert.strictEqual(r.str4GtStrCalc, true);
  assert.strictEqual(r.str4GtStrFromBytes, true);
});
