package main

import (
	"bytes"
	"io"
	"testing"
	"strings"
)

func TestPrintFirst(t *testing.T) {
	var r io.Reader
	r = bytes.NewBufferString("hello.\nthis is the first message for doing something new.")
    buf := &bytes.Buffer{}
	printFirst(r, buf)

    output := buf.String()
    if !strings.Contains(output, "hello") {
        t.Errorf("%q must contains %q", output, "hello")
    }
}
