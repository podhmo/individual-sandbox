package main

import (
	"log"

	"github.com/pkg/errors"
	"github.com/podhmo/maperr"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("! %+v", err)
	}
}

func run() error {
	if err := do(); err != nil {
		return errors.Wrap(err, "do")
	}
	return nil
}

func do() error {
	var err *maperr.Error
	return err.
		AddSummary("hello").
		Add("name", maperr.Message{Text: "hmm"}).
		Untyped()
}
