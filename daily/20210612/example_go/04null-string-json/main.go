package main

import (
	"database/sql"
	"encoding/json"
	"log"
	"os"

	"gopkg.in/guregu/null.v4"
	"gopkg.in/guregu/null.v4/zero"
)

type S struct {
	String      string         `json:"string"`
	NullString  sql.NullString `json:"sql.nullstring"`
	NullString2 null.String    `json:"null.nullstring"`
	NullString3 zero.String    `json:"zero.nullstring"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")

	if err := enc.Encode(S{
		String:      "",
		NullString:  sql.NullString{String: "", Valid: true},
		NullString2: null.NewString("", true),
		NullString3: zero.NewString("", true),
	}); err != nil {
		return err
	}

	if err := enc.Encode(S{
		String:      "",
		NullString:  sql.NullString{String: "", Valid: false},
		NullString2: null.NewString("", false),
		NullString3: zero.NewString("", false),
	}); err != nil {
		return err
	}
	return nil
}
