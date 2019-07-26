package main

import (
	"path/filepath"
	"fmt"
	"os"
	"testing"
)

func Test(t *testing.T) {
	f, err := os.Create(fmt.Sprintf("%s.golden", filepath.Join("testdata", t.Name())))
	if err != nil {
		t.Fatalf("prepare: %+v", err)
	}
	fmt.Fprintln(f, "hello")
}
