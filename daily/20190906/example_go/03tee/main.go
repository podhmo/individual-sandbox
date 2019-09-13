package main

import (
	"bytes"
	"io"
	"os"
)

func main() {
	b := bytes.NewBufferString("hello")
	var buf bytes.Buffer
	io.Copy(os.Stdout, io.TeeReader(b, &buf))
	io.Copy(os.Stdout, &buf)
}
