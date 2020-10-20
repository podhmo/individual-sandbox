package main

import (
	"bytes"
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"html/template"
	"log"
	"os"
	"strings"
)

const code = `
package main

type S struct {
// BLOCK: {{template "fields" .Fields }}
}
`

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	tmpl, err := build()
	if err != nil {
		return err
	}

	tmpl.AddParseTree("fields", template.Must(template.New("fields").Parse(`{{- range . }}
	{{ . }} string
{{- end -}}
`)).Tree)

	var data struct {
		Fields []string
	}
	var buf bytes.Buffer
	data.Fields = []string{"Foo", "Bar", "Boo"}
	if err := tmpl.Execute(&buf, data); err != nil {
		return err
	}
	b, err := format.Source(buf.Bytes())
	if err != nil {
		return err
	}
	if _, err := os.Stdout.Write(b); err != nil {
		return err
	}
	return nil
}

func build() (*template.Template, error) {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "", code, parser.ParseComments)
	if err != nil {
		return nil, err
	}

	for _, cg := range f.Comments {
		for _, c := range cg.List {
			if strings.Contains(c.Text, "// BLOCK:") {
				c.Text = strings.Replace(c.Text, "// BLOCK:", "", 1)
			}
		}
	}

	var buf bytes.Buffer
	if err := printer.Fprint(&buf, fset, f); err != nil {
		return nil, err
	}
	return template.New("ROOT").Parse(buf.String())
}
