package main

import (
	"encoding/json"
	"log"
	"os"
)

// NullBool ...
type NullBool struct {
	Bool  bool
	Valid bool
}

var (
	sNull  = []byte("null")
	sTrue  = []byte("true")
	sFalse = []byte("false")
)

func (b NullBool) MarshalJSON() ([]byte, error) {
	if !b.Valid {
		return sNull, nil
	}
	if b.Bool {
		return sTrue, nil
	}
	return sFalse, nil
}

type s struct {
	Value NullBool `json:"value"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(s{Value: NullBool{Bool: true, Valid: true}}); err != nil {
		return err
	}
	if err := encoder.Encode(s{Value: NullBool{Bool: false, Valid: true}}); err != nil {
		return err
	}
	if err := encoder.Encode(s{Value: NullBool{}}); err != nil {
		return err
	}
	return nil
}
