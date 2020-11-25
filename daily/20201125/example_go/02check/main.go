package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"text/template"

	"github.com/go-playground/validator"
)

// TODO: namespace?
// TODO: params -> interface{} ?

type Params struct {
	Items []string `validate:"required"`
}

type Candidates struct {
	OK []Params
	NG []Params
}

type Definition struct {
	New      func() *Params
	Examples Candidates
	Template string
}

var definition = Definition{
	New: func() *Params {
		var p Params
		return &p
	},
	Template: `{{- range $i, $x := .Items }}
- {{ $x -}}
{{ end }}
`,
	Examples: Candidates{
		OK: []Params{
			{Items: []string{"foo", "bar", "boo"}},
		},
		NG: []Params{
			{Items: nil},
		},
	},
}

func (d *Definition) Check(w io.Writer, validate *validator.Validate) error {
	tmpl, err := template.New("").Parse(d.Template)
	if err != nil {
		return fmt.Errorf("parse: %w", err)
	}

	if validate == nil {
		// todo: warning
		validate = validator.New()
	}

	for _, params := range d.Examples.OK {
		if err := validate.Struct(params); err != nil {
			return fmt.Errorf("validate: %w, ng with %+v", err, params)
		}
	}
	for _, params := range d.Examples.NG {
		if err := validate.Struct(params); err == nil {
			return fmt.Errorf("validate: %w, not ng with %+v", err, params)
		}
	}

	// OK
	for _, params := range d.Examples.OK {
		if err := tmpl.Execute(w, params); err != nil {
			return fmt.Errorf("validate: %w, ng with %+v", err, params)
		}
	}
	return nil
}

func (d *Definition) Execute(w io.Writer, validate *validator.Validate, params *Params) error {
	// TODO: cache
	tmpl, err := template.New("").Parse(d.Template)
	if err != nil {
		return fmt.Errorf("parse: %w", err)
	}

	if validate == nil {
		// todo: warning
		validate = validator.New()
	}

	if err := validate.Struct(params); err != nil {
		return fmt.Errorf("validate: %w, ng with %+v", err, params)
	}
	if err := tmpl.Execute(w, params); err != nil {
		return fmt.Errorf("execute: %w, ng with %+v", err, params)
	}
	return nil
}

func GetWriter() io.Writer {
	if ok, _ := strconv.ParseBool(os.Getenv("DEBUG")); ok {
		fmt.Println("ok")
		return os.Stdout
	}
	var b bytes.Buffer
	return &b
}

func main() {
	w := GetWriter()
	if err := definition.Check(w, nil); err != nil {
		log.Fatalf("%+v", err)
	}
	log.Println("check ok")
	params := definition.New()
	params.Items = []string{"foo", "bar", "boo"}
	if err := definition.Execute(os.Stdout, nil, params); err != nil {
		log.Fatalf("!! +v", err)
	}
}
