package main

import (
	"encoding/json"

	"github.com/davecgh/go-spew/spew"
)

// Pereson :
type Person struct {
	Name string
	Age  int
}

func main() {
	b := []byte(`{"Name": "foo", "Age": 20}`)
	var p Person
	json.Unmarshal(b, &p)
	spew.Dump(p)
}
