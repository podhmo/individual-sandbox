package main

import (
	"bytes"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"m/astmpl"
	"m/def2"
	"os"
	"path/filepath"
)

type Params struct {
	Name  string   `validate:"required"`
	Items []string `validate:"required"`
	Memo  *Memo
}

type Memo struct {
	Name string
}

func main() {
	tmpl, err := astmpl.Parse(filepath.Join(filepath.Dir(astmpl.FileName(def2.New)), "gen.go"))
	if err != nil {
		panic(err)
	}
	body, err := ParseBody(astmpl.FileName(main), "Params")
	if err != nil {
		panic(err)
	}
	tmpl.Execute(os.Stdout, map[string]interface{}{
		"Name":       "Foo",
		"NameParams": body,
		"Template": `
	{{ range $i, $x := .Items -}}
	- {{ $x }}
	{{ end -}}
	`,
	})
}

func ParseBody(filename, name string) (string, error) {
	fset := token.NewFileSet()
	t, err := parser.ParseFile(fset, filename, nil, parser.ParseComments)
	if err != nil {
		return "", err
	}
	var b bytes.Buffer
	decl := t.Scope.Lookup(name).Decl.(*ast.TypeSpec)
	if err := printer.Fprint(&b, fset, decl.Type); err != nil {
		return "", err
	}
	return b.String(), nil
}
