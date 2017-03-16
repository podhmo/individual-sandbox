package main

import (
	"encoding/json"
	"strings"

	"github.com/davecgh/go-spew/spew"
)

// D :
type D struct {
	X *int `json:"x"`
	Y *int `json:"y"`
}

func main() {
	data := `
[
  {"x": 10, "y": 10},
  {"x": 10, "y": 20},
  {"x": 30, "y": 30}
]
`
	var ob []*D
	r := strings.NewReader(data)
	decoder := json.NewDecoder(r)
	err := decoder.Decode(&ob)
	if err != nil {
		panic(err)
	}
	spew.Dump(ob)
}
