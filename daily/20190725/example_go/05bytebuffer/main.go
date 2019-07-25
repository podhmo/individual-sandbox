package main

import (
	"bytes"
	"fmt"
)

func main() {
	var b bytes.Buffer
	b.WriteString(`1,2,3,`)
	b.Truncate(b.Len() - 1)
	fmt.Println(b.String())
}
