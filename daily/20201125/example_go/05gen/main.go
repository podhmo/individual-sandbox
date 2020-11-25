package main

import (
	"html/template"
	"m/astmpl"
	"m/def"
	"os"
	"path/filepath"
)

func main() {
	tmpl := template.Must(astmpl.Parse(filepath.Join(filepath.Dir(astmpl.FileName(def.New)), "gen.go")))
	var b bytes.Buffer
	tmpl.Execute(&b, map[string]interface{}{
		"Name": "Foo",
		"NameBody": `
	Name string
	Items []string
`,
		"Template": `
{{ range $i, $x := .Items -}}
- {{ $x }}
{{ end -}}
`,
	})
}
