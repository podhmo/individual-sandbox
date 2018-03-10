package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
)

func main() {
	source := `x
y
`
	b := bytes.NewBufferString(source)
	fmt.Fprintln(os.Stdout, "z")
	io.Copy(os.Stdout, b)
	fmt.Println("----------------------------------------")
	io.Copy(os.Stdout, b)
}
