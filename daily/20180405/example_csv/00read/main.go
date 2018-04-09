package main

import (
	"bytes"
	"encoding/csv"

	"github.com/k0kubun/pp"
)

func main() {
	source := `
x,1

y
z,2,i
`
	r := csv.NewReader(bytes.NewBufferString(source))
	pp.ColoringEnabled = false
	for i := 0; i < 4; i++ {
		row, err := r.Read()
		pp.Println(i, row, err)
	}
}
