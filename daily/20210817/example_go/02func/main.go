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
	t, err := template.New("T").Funcs(template.FuncMap{
		"ToUpper": strings.ToUpper,
	}).ParseFiles("template/hello2.tmpl")
	if err != nil {
		return err
	}
	return t.ExecuteTemplate(os.Stdout, "hello2.tmpl", map[string]string{"Name": "World"})
}
