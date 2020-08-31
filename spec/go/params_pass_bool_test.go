// Autogenerated from KST: please remove this line if doing any edits by hand!

package spec

import (
	"runtime/debug"
	"os"
	"testing"
	"github.com/kaitai-io/kaitai_struct_go_runtime/kaitai"
	. "test_formats"
	"github.com/stretchr/testify/assert"
)

func TestParamsPassBool(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			debug.PrintStack()
			t.Fatal("unexpected panic:", r)
		}
	}()
	f, err := os.Open("../../src/term_strz.bin")
	if err != nil {
		t.Fatal(err)
	}
	s := kaitai.NewStream(f)
	var r ParamsPassBool
	err = r.Read(s, &r, &r)
	if err != nil {
		t.Fatal(err)
	}

	assert.EqualValues(t, false, r.SFalse)
	assert.EqualValues(t, true, r.STrue)
	assert.EqualValues(t, true, r.SeqB1.Arg)
	assert.EqualValues(t, 1, len(r.SeqB1.Foo))
	assert.EqualValues(t, false, r.SeqBool.Arg)
	assert.EqualValues(t, 2, len(r.SeqBool.Foo))
	assert.EqualValues(t, false, r.LiteralB1.Arg)
	assert.EqualValues(t, 2, len(r.LiteralB1.Foo))
	assert.EqualValues(t, true, r.LiteralBool.Arg)
	assert.EqualValues(t, 1, len(r.LiteralBool.Foo))
	assert.EqualValues(t, true, r.InstB1.Arg)
	assert.EqualValues(t, 1, len(r.InstB1.Foo))
	assert.EqualValues(t, false, r.InstBool.Arg)
	assert.EqualValues(t, 2, len(r.InstBool.Foo))
}
