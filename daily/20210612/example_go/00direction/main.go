package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strings"
)

type SortKey struct {
	Field string
	Desc  bool
}

func (sk *SortKey) UnmarshalText(b []byte) error {
	if len(b) == 0 {
		return nil
	}

	if b[0] == '-' {
		sk.Desc = true
		sk.Field = string(b[1:])
	} else {
		sk.Field = string(b)
	}
	return nil
}
func (sk SortKey) MarshalText() ([]byte, error) {
	if sk.Desc {
		return []byte("-" + sk.Field), nil
	}
	return []byte(sk.Field), nil
}

func ParseSortKey(s string) (SortKey, error) {
	var sk SortKey
	if s == "" {
		return sk, nil
	}
	if s[0] == '-' {
		sk.Field = s[1:]
		sk.Desc = true
		return sk, nil
	}
	sk.Field = s
	return sk, nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	log.Println("encode")
	{
		keys := []SortKey{
			{Field: "age", Desc: true},
			{Field: "pk"},
		}

		fmt.Printf("input: %#+v\n", keys)
		fmt.Printf("output: ")
		enc := json.NewEncoder(os.Stdout)
		if err := enc.Encode(keys); err != nil {
			return err
		}
	}

	log.Println("decode")
	{
		text := `["-name", "age"]`
		fmt.Printf("input: %q\n", text)
		dec := json.NewDecoder(strings.NewReader(text))
		keys := make([]SortKey, 0, 2)
		if err := dec.Decode(&keys); err != nil {
			return err
		}
		fmt.Printf("output: %#+v\n", keys)
	}
	return nil
}
