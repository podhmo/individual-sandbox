package main

import (
	"log"
	"os"
	"text/template"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	t, err := template.ParseFiles("template/hello.tmpl")
	if err != nil {
		return err
	}
	return t.Execute(os.Stdout, map[string]string{"Name": "World"})
}
