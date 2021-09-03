package main

import (
	"log"
	"os"
	"strings"
	"text/template"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	// {
	// 	// It must be called before the template is parsed.
	// 	t, err := template.ParseFiles("template/hello2.tmpl")
	// 	if err != nil {
	// 		return err
	// 	}
	// 	t = t.Funcs(template.FuncMap{
	// 		"ToUpper": strings.ToUpper,
	// 	})
	// 	return t.Execute(os.Stdout, map[string]string{"Name": "World"})
	// }

	t, err := template.New("TXXX").Funcs(template.FuncMap{
		"ToUpper": strings.ToUpper,
	}).Parse(`Hello {{ .Name | ToUpper }}
`)
	if err != nil {
		return err
	}
	return t.Execute(os.Stdout, map[string]string{"Name": "World"})
}
