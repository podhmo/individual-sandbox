package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strings"
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

func (v NullBool) MarshalJSON() ([]byte, error) {
	if !v.Valid {
		return sNull, nil
	}
	if v.Bool {
		return sTrue, nil
	}
	return sFalse, nil
}
func (v *NullBool) UnmarshalJSON(data []byte) error {
	switch string(data) {
	case "null":
		return nil
	case "true":
		v.Bool = true
		v.Valid = true
		return nil
	case "false":
		v.Valid = true
		return nil
	default:
		return fmt.Errorf("invalid string: %q", data)
	}
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
	use := func(s string) error {
		var v NullBool
		decoder := json.NewDecoder(strings.NewReader(s))
		if err := decoder.Decode(&v); err != nil {
			return err
		}
		fmt.Printf("%+v\n", v)
		return nil
	}
	if err := use("true"); err != nil {
		return err
	}
	if err := use("false"); err != nil {
		return err
	}
	if err := use("null"); err != nil {
		return err
	}
	if err := use(`"あ"`); err != nil {
		return err
	}
	return nil
}
