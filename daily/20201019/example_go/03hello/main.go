package main

import (
	"html/template"
	"os"
)

func main() {
	tmpl := template.Must(template.New("ROOT").Parse(`Hello {{.Thing}}`))
	data := struct{ Thing string }{"World"}
	tmpl.Execute(os.Stdout, data)
}
