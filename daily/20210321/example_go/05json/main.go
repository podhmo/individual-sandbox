package main

import (
	"encoding/json"
	"os"
)

type X struct {
	Name   string `json:"name"`
	Values []int  `json:"values"`
}

func main() {
	{
		x := X{Name: "x"}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		enc.Encode(x)
	}
	{
		x := X{Name: "x", Values: []int{}}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		enc.Encode(x)
	}
}
