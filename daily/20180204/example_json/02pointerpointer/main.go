package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"

	"github.com/k0kubun/pp"
)

// X :
type X struct {
	V **int `json:"v"`
}

func main() {
	s := `[
  {},
  {"v": 1}
]`
	var xs []X
	decoder := json.NewDecoder(bytes.NewBufferString(s))
	if err := decoder.Decode(&xs); err != nil {
		log.Fatal(err)
	}
	pp.Println(xs)
	fmt.Println(**xs[1].V)
}
