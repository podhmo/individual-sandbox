package main

import (
	"database/sql"
	"database/sql/driver"
	"fmt"
	"log"

	"github.com/guregu/null"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func use(v driver.Valuer) error {
	ob, err := v.Value()
	if err != nil {
		return err
	}
	switch ob.(type) {
	case string:
		fmt.Println("YAY")
	default:
		fmt.Println("hmm", ob)
	}
	return nil
}

func run() error {
	{
		ns := sql.NullString{String: "", Valid: true}
		if err := use(ns); err != nil {
			return err
		}
	}
	{
		ns := null.NewString("", true)
		if err := use(ns); err != nil {
			return err
		}

	}
	return nil
}
