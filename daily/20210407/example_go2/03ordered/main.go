package main

import (
	"encoding/json"
	"log"
	"os"

	ordered "gitlab.com/c0b/go-ordered-json"
)

type OMap ordered.OrderedMap

var NewOMap = ordered.NewOrderedMap

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	m := NewOMap()
	m.Set("name", "foo")
	m.Set("age", 10)

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(m)
}
