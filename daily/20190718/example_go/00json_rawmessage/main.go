package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
)

func main() {
	if len(os.Args) <= 1 {
		fmt.Fprintln(os.Stderr, "<program> [target file]")
		os.Exit(1)
	}

	if err := run(os.Args[1]); err != nil {
		log.Fatal(err)
	}
}

func run(filename string) error {
	f, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer f.Close() // xxx: close error

	type wrap struct {
		Wrap json.RawMessage `json:"wrap"`
	}
	var w wrap
	decoder := json.NewDecoder(f)
	if err := decoder.Decode(&w.Wrap); err != nil {
		return err
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(&w)
}
