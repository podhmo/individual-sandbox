package main

import (
	"log"
	"os"
	"text/template"
)

func main() {
	tmpl := template.Must(template.New("").Parse(`
{{- range $i, $x := .Items }}
- {{ $x -}}
{{ end }}
`))
	var params struct {
		Items []string
	}
	params.Items = []string{"foo", "bar", "boo"}

	if err := tmpl.Execute(os.Stdout, params); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
