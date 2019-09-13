package main

import (
	"bytes"
	"io"
	"os"
)

func main() {
	w := io.MultiWriter(os.Stdout, os.Stderr)
	b := bytes.NewBufferString("hello\n")
	io.Copy(w, b)
}
