package main

import (
	"io/ioutil"
	"os"
	"testing"
)

func setup(t *testing.T) (*os.File, func(), error) {
	teardown := func() {}

	f, err := ioutil.TempFile("", "test")
	if err != nil {
		return nil, teardown, err
	}
	teardown = func() {
		err := f.Close()
		if err != nil {
			t.Error("setup: Close:", err)
		}
		err = os.RemoveAll(f.Name())
		if err != nil {
			t.Error("setup: RemoveAll:", err)
		}
	}
	return f, teardown, nil
}

func main() {
	var t *testing.T
	f, teardown, err := setup(t)
	defer teardown()
	if err != nil {
		t.Error("setup:", err)
	}
	// do something with f
}
