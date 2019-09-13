package main

import (
	"bytes"
	"fmt"
	"io"
)

func main() {
	b := bytes.NewBufferString("hello\n")
	r, w := io.Pipe()
	go func() {
		io.Copy(w, b)
		defer w.Close()
	}()
	var b2 bytes.Buffer
	b2.ReadFrom(r)
	fmt.Println(b2.String())
}
