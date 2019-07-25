package main

import (
	"encoding/json"
	"log"
	"net/http"
	"os"
)

func main() {
	url := os.Args[1]
	if err := run(url); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(url string) error {
	res, err := http.Get(url)
	if err != nil {
		return err
	}

	val := struct {
		Wrap *json.RawMessage `json:"wrap"`
	}{}

	defer res.Body.Close()
	decoder := json.NewDecoder(res.Body)
	if err := decoder.Decode(&val.Wrap); err != nil {
		return nil
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetEscapeHTML(false)
	encoder.SetIndent("", "  ")

	return encoder.Encode(&val)
}
