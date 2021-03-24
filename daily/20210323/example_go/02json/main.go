package main

import (
	"encoding/json"
	"log"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type s struct {
	x int `json:"x"`
	Y int `json:"y"`
}

func run() error {
	json.NewEncoder(os.Stdout).Encode(s{})
	return nil
}
