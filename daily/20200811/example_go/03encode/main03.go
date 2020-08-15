package main

import (
	"io"
	"log"
	"os"

	"github.com/goccy/go-json"
)

type Person struct {
	Name   string  `json:"name"`
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
		Father: &Person{
			Name: "bar",
		},
	}
	encoder := json.NewEncoder(o)
	encoder.SetIndent("", "  ")
	return runInner(p, encoder)
}

func runInner(p *Person, encoder *json.Encoder) error {
	return encoder.Encode(p)
}
