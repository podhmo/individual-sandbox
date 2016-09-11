package main

import (
	"go/parser"
	"go/token"
	"io/ioutil"
	"log"
)

func main() {
	templateName := "./greeting.go"
	b, err := ioutil.ReadFile(templateName)
	if err != nil {
		log.Fatal(err)
	}

	fset := token.NewFileSet()
	source := string(b)
	// f, err := parser.ParseFile(fset, templateName, source, parser.Trace)
	f, err := parser.ParseFile(fset, templateName, source, 0)
	if err != nil {
		log.Fatal(err)
	}

	log.Printf("----------------------------------------")
	log.Printf("result: %[1]T, %+#[1]v\n", f)
	log.Printf("----------------------------------------")
}
