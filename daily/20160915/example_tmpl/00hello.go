package main

import (
	"fmt"
	"os"
	"text/template"
)

type User struct {
	Name    string
	Message string
}
type V map[string]interface{}

func main() {

	const tmpl = `{{.user.Name}}: {{.user.Message}}`

	tpl := template.Must(template.New("mytemplate").Parse(tmpl))

	user := User{Name: "foo", Message: "hello world"}

	if err := tpl.Execute(os.Stdout, V{"user": user}); err != nil {
		fmt.Println(err)
	}
}
