package main

import (
	"os"

	"github.com/goccy/go-json"
)

type S0 struct {
	X S1 `json:"x"`
	Y S1 `json:"y"`
}

type S1 struct {
	Name string `json:"name"`
}

func main() {
	s := S0{X: S1{Name: "foo"}, Y: S1{Name: "bar"}}
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	encoder.Encode(&s)
}
