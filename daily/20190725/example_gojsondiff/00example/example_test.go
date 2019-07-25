package main

import (
	"bytes"
	"m/jsonequal"
	"testing"
)

func Test(t *testing.T) {
	v := map[string]int{"foo": 1}
	b := []byte(`{"foo": 1}`)
	r := bytes.NewBufferString(`{"foo": 1}`)

	if err := jsonequal.Equal(jsonequal.From(v), jsonequal.FromBytes(b)); err != nil {
		t.Errorf("mismatch: %s", err)
	}

	if err := jsonequal.Equal(jsonequal.From(v), jsonequal.FromReader(r)); err != nil {
		t.Errorf("mismatch: %s", err)
	}

	if err := jsonequal.Equal(jsonequal.FromBytes([]byte(`{"boo": 1}`)), jsonequal.From(v)); err != nil {
		t.Errorf("mismatch: %s", err)
	}
}
