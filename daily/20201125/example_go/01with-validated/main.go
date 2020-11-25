package main

import (
	"log"
	"os"
	"text/template"

	"github.com/go-playground/validator"
)

// TODO: broken template check in compile time

func main() {
	validate := validator.New()

	tmpl := template.Must(template.New("").Parse(`
{{- range $i, $x := .Items }}
- {{ $x -}}
{{ end }}
`))
	var params struct {
		Items []string `validate:"required"`
	}
	params.Items = []string{"foo", "bar", "boo"}

	if err := validate.Struct(params); err != nil {
		log.Fatalf("validate: %+v", err)
		os.Exit(1)
	}

	if err := tmpl.Execute(os.Stdout, params); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
