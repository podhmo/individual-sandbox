package main

import (
	"context"
	"m/validation"
	"strings"
)

type Name string
type Person struct {
	Name   string `json:"name"`
	Father *Person
}

func (p Person) Validation() []validation.FieldValidation {
	return []validation.FieldValidation{
		validation.Field("Name", validation.Required()),
		validation.Field("Father"), // TODO: auto
	}
}

var zString string

func (p *Person) Validate(ctx context.Context, path []string) error {
	var errs FieldErrorList
	if p.Name == zString { // required
		errs = append(errs, FieldError{Path: strings.Join(path, "/") + "/Name", Value: p.Name})
	}
	if errs != nil {
		return errs
	}
	return nil
}

type FieldError struct {
	Path  string
	Value interface{}
}

type FieldErrorList []FieldError

func (xs FieldErrorList) Error() string { return "<E ... TODO>" }

func main() {
	c := validation.New()
	c.Debug = true
	c.Visit(&Person{})

	// enc := json.NewEncoder(os.Stdout)
	// enc.SetIndent("", "  ")
	// enc.Encode(c.CommandEmitter.Command)
}
