package main

import (
	"bytes"
	"fmt"
)

func main() {
	var buf bytes.Buffer
	buf.WriteString("foo,bar,boo")
	fmt.Println(buf.String())
	buf.Truncate(buf.Len() - 4)
	fmt.Println(buf.String())
}
