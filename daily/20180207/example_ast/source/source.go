package source

import (
	"go/ast"
	"go/parser"
	"go/token"
)

// Config :
type Config struct {
	Fset  *token.FileSet
	Files []*ast.File
}

// Build :
func Build() (*Config, error) {
	codeset := []string{
		`
package p
import "time"

// A of before
type A struct {
	Name string
	Before time.Time
}

// B of before
type B struct {
	Name string
	Before time.Time
}
`,
		`
package p
import "time"

// A of after
type A struct {
	After time.Time
	Name string
}

// B of after
type B struct {
	After time.Time
	Name string
}
`,
	}

	fset := token.NewFileSet()
	var files []*ast.File
	for _, code := range codeset {
		file, err := parser.ParseFile(fset, "", code, parser.ParseComments)
		if err != nil {
			return nil, err
		}
		files = append(files, file)
	}
	return &Config{Fset: fset, Files: files}, nil
}
