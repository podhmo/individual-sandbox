package main

import (
	"os"
	"text/template"
)

func main() {
	tmpl := template.Must(template.New("ROOT").Parse("hello {{ . }}"))
	tmpl.Execute(os.Stdout, "world")
}
