package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
)

func main() {
	var j bytes.Buffer
	io.Copy(&j, os.Stdin)
	var dst bytes.Buffer

	if err := json.Indent(&dst, j.Bytes(), "", "    "); err != nil {
		fmt.Fprintf(os.Stderr, "pretty: %v", err)
		os.Exit(-1)
	}
	fmt.Println(dst.String())
}
