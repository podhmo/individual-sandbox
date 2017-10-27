package main

import (
	"encoding/json"
	"log"
	"os"
)

type p struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
    alias string
}

func main() {
	encoder := json.NewEncoder(os.Stdout)
	ob := p{Name: "foo", Age: 20, alias: "f"}
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(ob); err != nil {
		log.Fatal(err)
	}
}
