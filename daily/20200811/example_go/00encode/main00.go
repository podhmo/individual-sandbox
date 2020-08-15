package main

import (
	"encoding/json"
	"io"
	"log"
	"os"
)

type Person struct {
	Name   string  `json:"name"`
	Age    int     `json:"age"`
	Father *Person `json:"father,omitempty"`
	Mother *Person `json:"mother,omitempty"`
}

func main() {
	if err := run(os.Stdout); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(o io.Writer) error {
	p := &Person{
		Name: "foo",
		Age:  20,
		Father: &Person{
			Name: "bar",
			Age:  40,
		},
	}
	encoder := json.NewEncoder(o)
	encoder.SetIndent("", "  ")
	return runInner(p, encoder)
}

func runInner(p *Person, encoder *json.Encoder) error {
	return encoder.Encode(p)
}
