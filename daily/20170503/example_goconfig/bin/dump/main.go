package main

import (
	"encoding/json"
	"log"
	"os"

	"github.com/k0kubun/pp"
	jsref "github.com/lestrrat/go-jsref"
)

// T :
type T struct {
	Type       string
	Properties map[string]T
	Required   []string
}

func run(filename string, jsptr string) error {
	f, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer f.Close()

	decoder := json.NewDecoder(f)
	var ob map[string]interface{}
	if err := decoder.Decode(&ob); err != nil {
		return err
	}

	ref := jsref.New()
	result, err := ref.Resolve(ob, jsptr)
	if err != nil {
		return err
	}
	pp.Print(result)
	return nil
}

func main() {
    // go run <this> ./swagger.json "#/definitions/bridge"
	if len(os.Args) <= 2 {
		os.Exit(1)
	}
	err := run(os.Args[1], os.Args[2])
	if err != nil {
		log.Fatal(err)
	}
}
