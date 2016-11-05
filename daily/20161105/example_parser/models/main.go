package main

import (
	"fmt"
	"go/parser"
	"go/token"
	"log"
	"path"

	"flag"

	"encoding/json"
	"strconv"
)

var target *string

func init() {
	target = flag.String("target", "", "target file")
}

func main() {
	flag.Parse()
	log.Println(*target)
	if *target == "" {
		log.Fatal("app -target <file>")
	}
	run(*target)
}

func scanImportsRec(m map[string]string, fname string) error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, fname, nil, 0)
	if err != nil {
		return err
	}
	for _, spec := range f.Imports {
		pathvalue, err := strconv.Unquote(spec.Path.Value)
		if err != nil {
			log.Fatal(err)
		}
		name := path.Base(pathvalue)
		if spec.Name != nil {
			name = spec.Name.Name
		}
		_, exists := m[name]
		if !exists {
			m[name] = pathvalue
		}
	}
	return nil
}

func run(fname string) {
	m := make(map[string]string)
	err := scanImportsRec(m, fname)
	if err != nil {
		log.Fatal(err)
	}
	b, err := json.Marshal(m)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(b))
}
