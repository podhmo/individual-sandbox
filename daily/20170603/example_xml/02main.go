package main

import (
	"html"
	"log"
	"os"
	"strings"
	"text/template"
)

func main() {
	tmpl := template.Must(template.New("template").Parse(strings.TrimSpace(`
<?xml version="1.0" encoding="utf-8"?>
<content>{{.}}</content>
`)))

	content := `&foo とか&が書かれた文章`
	if err := tmpl.Execute(os.Stdout, html.EscapeString(content)); err != nil {
		log.Fatal(err)
	}
}
