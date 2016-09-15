package main

import (
	"fmt"
	"os"
	"text/template"
)

type User struct {
	name    string
	Message string
}
type V map[string]interface{}

func main() {

	const tmpl = `{{.user.name}}: {{.user.Message}}`

	tpl := template.Must(template.New("mytemplate").Parse(tmpl))

	user := User{name: "foo", Message: "hello world"}

	// template: mytemplate:1:7: executing "mytemplate" at <.user.name>: name is an unexported field of struct type interface {}
	if err := tpl.Execute(os.Stdout, V{"user": user}); err != nil {
		fmt.Println(err)
	}
}
