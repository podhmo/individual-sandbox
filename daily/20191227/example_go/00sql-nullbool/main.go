package main

import (
	"database/sql"
	"encoding/json"
	"log"
	"os"
)

type s struct {
	Value sql.NullBool `json:"value"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(s{Value: sql.NullBool{Bool: true, Valid: true}}); err != nil {
		return err
	}
	return nil
}
