package main

import (
	"encoding/json"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
	"os"
)

var source = `
package m

type Person struct {Name string}
func NewPerson(name string) *Person {
	return &Person{Name: string}
}


type Importer = interface{}
type Package = interface{}
type File = interface{}
type NewPackage = func(fset *token.FileSet, files map[string]*File, importer Importer, importers ...Importer) (*Package, error)
`

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

type visiter struct{}

func (v *visiter) Visit(node ast.Node) ast.Visitor {
	fmt.Printf("%#+v\n", node)
	return v
}

func run() error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "xxx.go", source, 0)
	if err != nil {
		return err
	}

	var r []Func
	for _, decl := range f.Decls {
		switch decl := decl.(type) {
		case *ast.FuncDecl:
			params := CollectFields(fset, decl.Type.Params)
			results := CollectFields(fset, decl.Type.Results)
			r = append(r, Func{
				Name:    decl.Name.Name,
				Params:  params,
				Results: results,
			})
		}
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(r)
}

type Func struct {
	Recv    *string `json:"recv,omitempty"`
	Name    string  `json:"name"`
	Params  []Field `json:"params"`
	Results []Field `json:"results"`
}

type Field struct {
	Name string `json:"name,omitempty"`
	Type Type   `json:"type"`
	// Raw  *ast.Field
}
type Type = interface{}

func CollectFields(fset *token.FileSet, fieldlist *ast.FieldList) []Field {
	var r []Field
	for _, field := range fieldlist.List {
		field := field
		names := field.Names
		if len(names) == 0 {
			names = append(names, &ast.Ident{Name: ""})
		}
		for _, name := range names {
			r = append(r, Field{
				Name: name.Name,
				Type: CollectType(fset, field.Type),
				// Raw:  field,
			})
		}
	}
	return r
}

func CollectType(fset *token.FileSet, node ast.Node) Type {
	m := make(map[string]Type)
	switch node := node.(type) {
	case *ast.Ident:
		m["kind"] = "primitive"
		m["value"] = node.Name
	case *ast.ArrayType:
		m["kind"] = "array"
		m["value"] = CollectType(fset, node.Elt)
	case *ast.MapType:
		m["kind"] = "map"
		m["key"] = CollectType(fset, node.Key)
		m["value"] = CollectType(fset, node.Value)
	case *ast.StructType:
		m["kind"] = "struct"
		m["fields"] = CollectType(fset, node.Fields)
	case *ast.InterfaceType:
		m["kind"] = "interface"
		m["methods"] = CollectType(fset, node.Methods)
	case *ast.StarExpr:
		m["kind"] = "pointer"
		m["value"] = CollectType(fset, node.X)
	case *ast.SelectorExpr:
		m["kind"] = "selector"
		m["prefix"] = node.X.(*ast.Ident).Name
		m["value"] = node.Sel.Name
	case *ast.FuncType:
		m["kind"] = "func"
		m["args"] = CollectType(fset, node.Params)
		m["results"] = CollectType(fset, node.Results)
	case *ast.TypeSpec:
		return CollectType(fset, node.Type)
	case *ast.ChanType:
		m["kind"] = "channel"
		m["value"] = CollectType(fset, node.Value)
		m["dir"] = node.Dir
		// xxx
	case *ast.FieldList:
		if node == nil {
			return []Type{}
		}
		args := make([]Type, len(node.List))
		for i, arg := range node.List {
			args[i] = CollectType(fset, arg.Type)
		}
		return args
	case *ast.Ellipsis:
		m["kind"] = "ellipsis"
		m["value"] = CollectType(fset, node.Elt)
	default:
		ast.Print(fset, node)
		panic(node)
	}
	return m
}

/*
	// A FuncDecl node represents a function declaration.
	FuncDecl struct {
		Doc  *CommentGroup // associated documentation; or nil
		Recv *FieldList    // receiver (methods); or nil (functions)
		Name *Ident        // function/method name
		Type *FuncType     // function signature: parameters, results, and position of "func" keyword
		Body *BlockStmt    // function body; or nil for external (non-Go) function
	}

	FuncType struct {
		Func    token.Pos  // position of "func" keyword (token.NoPos if there is no "func")
		Params  *FieldList // (incoming) parameters; non-nil
		Results *FieldList // (outgoing) results; or nil
	}

	// A FieldList represents a list of Fields, enclosed by parentheses or braces.
	FieldList struct {
		Opening token.Pos // position of opening parenthesis/brace, if any
		List    []*Field  // field list; or nil
		Closing token.Pos // position of closing parenthesis/brace, if any
	}

	Field struct {
		Doc     *CommentGroup // associated documentation; or nil
		Names   []*Ident      // field/method/parameter names; or nil
		Type    Expr          // field/method/parameter type
		Tag     *BasicLit     // field tag; or nil
		Comment *CommentGroup // line comments; or nil
	}

	Ident struct {
		NamePos token.Pos // identifier position
		Name    string    // identifier name
		Obj     *Object   // denoted object; or nil
	}
*/
