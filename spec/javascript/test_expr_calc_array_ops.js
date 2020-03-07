// Autogenerated from KST: please remove this line if doing any edits by hand!

var assert = require('assert');
var testHelper = require('testHelper');

testHelper('ExprCalcArrayOps', 'src/fixed_struct.bin', function(r, ExprCalcArrayOps) {

  assert.strictEqual(r.intArraySize, 7);
  assert.strictEqual(r.intArrayFirst, 10);
  assert.strictEqual(r.intArrayMid, 25);
  assert.strictEqual(r.intArrayLast, 1000);
  assert.strictEqual(r.intArrayMin, 10);
  assert.strictEqual(r.intArrayMax, 1000);
  assert.strictEqual(r.doubleArraySize, 5);
  assert(Math.abs(r.doubleArrayFirst - 10.0) < 1e-6);
  assert(Math.abs(r.doubleArrayMid - 25.0) < 1e-6);
  assert(Math.abs(r.doubleArrayLast - 3.14159) < 1e-6);
  assert(Math.abs(r.doubleArrayMin - 3.14159) < 1e-6);
  assert(Math.abs(r.doubleArrayMax - 100.0) < 1e-6);
  assert.strictEqual(r.strArraySize, 4);
  assert.strictEqual(r.strArrayFirst, "un");
  assert.strictEqual(r.strArrayMid, "deux");
  assert.strictEqual(r.strArrayLast, "quatre");
  assert.strictEqual(r.strArrayMin, "deux");
  assert.strictEqual(r.strArrayMax, "un");
});