package main

import (
	"log"
	"os"
	"text/template"
)

type Thing struct {
	Text   string
	Things []Thing
}

func main() {
	w := os.Stdout
	tmpl, err := template.New("root").Parse(`{{ .Text }}

{{ range .Things -}}
- {{ .Text }}
{{ end -}}

{{ define "listing" }}
{{ range . -}}
- {{ .Text }}
{{ end -}}
{{ end -}}

{{ template "listing" .Things }}
`)
	data := Thing{
		Text: "Hello",
		Things: []Thing{
			{
				Text: "World",
			},
			{
				Text: "World?",
			},
		},
	}
	err = tmpl.Execute(w, data)
	if err != nil {
		log.Println("Error:", err)
	}
}
