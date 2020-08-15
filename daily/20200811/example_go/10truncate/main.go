package main

import (
	"bytes"
	"io"
	"os"
)

func main() {
	b := bytes.NewBufferString(`@xxx@`)
	b.Truncate(b.Len() - 1)
	io.Copy(os.Stdout, b)
}
