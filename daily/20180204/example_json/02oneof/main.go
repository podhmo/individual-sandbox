package main

import (
	"encoding/json"
	"os"
)

// Item :
type Item struct {
	Type string `json:"type"` // a or b
	A    *A     `json:"a,omitempty"`
	B    *B     `json:"b,omitempty"`
}

// A :
type A struct {
	Value string `json:"value"`
}

// B :
type B struct {
	Value string `json:"value"`
}

func main() {
	item := Item{Type: "a", A: &A{Value: "this is a"}}
	encoder := json.NewEncoder(os.Stdout)
	encoder.Encode(&item)
}
