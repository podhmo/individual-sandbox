package main

import (
	"log"

	"github.com/podhmo/validator"
)

type S struct {
	Name string `validate:"@pattern=^[A-Z]+"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	c := &validator.Config{
		Tag: "validate",
	}
	c.InstallFieldValidation("pattern", validator.Regexp())
	v, err := c.BuildValidator()
	if err != nil {
		return err
	}

	return v.Validate(&S{Name: "foo"})
}
