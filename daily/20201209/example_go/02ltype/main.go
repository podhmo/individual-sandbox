package main

import (
	"encoding/json"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
	"strings"
)

type Func struct {
	Name    string   `json:"name"`
	Params  []Symbol `json:"params"`
	Returns []Symbol `json:"returns"`
	Raw     struct {
		Recv    *ast.FieldList
		Params  *ast.FieldList
		Returns *ast.FieldList
	} `json:"-"`
	// TODO: pkg
}
type Symbol struct {
	Name string `json:"name"`
	Type Type   `json:"type"`
}
type Kind struct {
	Kind string
}
type Type interface {
	Type() string
}
type Basic struct {
	Kind
	Name string
}

func (v *Basic) Type() string {
	return v.Name
}

type Dotted struct {
	Kind
	Name     string
	Imported string
}

func (v *Dotted) Type() string {
	return fmt.Sprintf("%s.%s", v.Imported, v.Name)
}

type Container struct {
	Kind
	Name string
	Args []Type
}

func (v *Container) Type() string {
	args := make([]string, len(v.Args))
	for i, x := range v.Args {
		args[i] = x.Type()
	}
	return fmt.Sprintf("%s[%s]", v.Name, strings.Join(args, ", "))
}

const code = `package foo
func Dup(s string) string {
	return s + s
}
func Dup2(ctx context.Context, s string) string {
	return s + s
}
func Dup3(ctx context.Context, s string) (string, error) {
	return s + s, nil
}
func Dup4(ctx context.Context, s,s1 string) (s string, err error) {
	return s + s, nil
}
func Sum(xs []int) int {
	n := 0
	for _, x := range xs {
		n += x
	}
	return n
}
func Sum2(xs ...int) int {
	n := 0
	for _, x := range xs {
		n += x
	}
	return n
}
func IncAll(c map[string]int, d int){
	for k := range c {
		c[k] += d
	}
}
`

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Extract(f *ast.File, name string, fn *Func) error {
	ob := f.Scope.Lookup(name)
	if ob == nil {
		return fmt.Errorf("target %q is not found", ob)
	}

	decl, ok := ob.Decl.(*ast.FuncDecl)
	if !ok {
		return fmt.Errorf("unexpected decl %T", ob)
	}

	var walk func(ast.Node, *Func) error
	var walkType func(ast.Node) (Type, error)

	walk = func(node ast.Node, fn *Func) error {
		switch node := node.(type) {
		case *ast.FuncDecl:
			fn.Name = node.Name.Name
			return walk(node.Type, fn)
		case *ast.FuncType:
			fn.Raw.Params = node.Params
			fn.Raw.Returns = node.Results
			if node.Params != nil {
				var symbols []Symbol
				for i, x := range node.Params.List {
					// todo: unnamed argument
					val, err := walkType(x.Type)
					if err != nil {
						return fmt.Errorf("on walk, walk params, %d, %w", i, err)
					}

					if len(x.Names) == 0 {
						symbols = append(symbols, Symbol{Type: val})
						continue
					}
					for _, ident := range x.Names {
						symbols = append(symbols, Symbol{Name: ident.Name, Type: val})
					}
				}
				fn.Params = symbols
			}

			if node.Results != nil {
				var symbols []Symbol
				for i, x := range node.Results.List {
					// todo: unnamed argument
					val, err := walkType(x.Type)
					if err != nil {
						return fmt.Errorf("on walk, walk results, %d, %w", i, err)
					}
					var name string
					if len(x.Names) > 0 {
						name = x.Names[0].Name
					}
					symbols = append(symbols, Symbol{Name: name, Type: val})
				}
				fn.Returns = symbols
			}

			return nil
		default:
			return fmt.Errorf("on walk, unexpected type: %T", node)
		}
	}
	walkType = func(node ast.Node) (Type, error) {
		switch node := node.(type) {
		// case *ast.BadExpr:
		case *ast.Ident:
			return &Basic{
				Kind: Kind{Kind: "basic"},
				Name: node.Name,
			}, nil
		case *ast.SelectorExpr:
			return &Dotted{
				Kind:     Kind{Kind: "dotted"},
				Imported: node.X.(*ast.Ident).Name,
				Name:     node.Sel.Name,
			}, nil
		case *ast.ArrayType:
			v, err := walkType(node.Elt)
			if err != nil {
				return nil, fmt.Errorf("array elt, %w", err)
			}
			return &Container{
				Kind: Kind{Kind: "container"},
				Name: "Array",
				Args: []Type{v},
			}, nil
		case *ast.Ellipsis:
			v, err := walkType(node.Elt)
			if err != nil {
				return nil, fmt.Errorf("ellipsis elt, %w", err)
			}
			return &Container{
				Kind: Kind{Kind: "container"},
				Name: "VarArg",
				Args: []Type{v},
			}, nil
		case *ast.MapType:
			k, err := walkType(node.Key)
			if err != nil {
				return nil, fmt.Errorf("map key, %w", err)
			}
			v, err := walkType(node.Value)
			if err != nil {
				return nil, fmt.Errorf("map value, %w", err)
			}
			return &Container{
				Kind: Kind{Kind: "container"},
				Name: "Map",
				Args: []Type{k, v},
			}, nil
		// case *ast.BasicLit:
		// case *ast.FuncLit:
		// case *ast.CompositeLit:
		// case *ast.ParenExpr:
		// case *ast.IndexExpr:
		// case *ast.SliceExpr:
		// case *ast.TypeAssertExpr:
		// case *ast.CallExpr:
		// case *ast.StarExpr:
		// case *ast.UnaryExpr:
		// case *ast.BinaryExpr:
		// case *ast.KeyValueExpr:

		// case *ast.StructType:
		// case *ast.FuncType:
		// case *ast.InterfaceType:
		// case *ast.ChanType:
		default:
			return nil, fmt.Errorf("on walkType, unexpected type: %T", node)
		}
	}
	return walk(decl, fn)
}

func run() error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "foo.go", code, parser.ParseComments)
	if err != nil {
		return err
	}
	{
		var fn Func
		if err := Extract(f, "Dup", &fn); err != nil {
			return fmt.Errorf("Dup, %w", err)
		}
		b, err := json.Marshal(fn)
		fmt.Printf("RESULT: %s,%v\n", b, err)
	}
	{
		var fn Func
		if err := Extract(f, "Dup2", &fn); err != nil {
			return fmt.Errorf("Dup2, %w", err)
		}
		b, err := json.Marshal(fn)
		fmt.Printf("RESULT: %s,%v\n", b, err)
	}
	{
		var fn Func
		if err := Extract(f, "Dup3", &fn); err != nil {
			return fmt.Errorf("Dup3, %w", err)
		}
		b, err := json.Marshal(fn)
		fmt.Printf("RESULT: %s,%v\n", b, err)
	}
	{
		var fn Func
		if err := Extract(f, "Dup4", &fn); err != nil {
			return fmt.Errorf("Dup4, %w", err)
		}
		b, err := json.Marshal(fn)
		fmt.Printf("RESULT: %s,%v\n", b, err)
	}
	{
		var fn Func
		if err := Extract(f, "Sum", &fn); err != nil {
			return fmt.Errorf("Sum, %w", err)
		}
		b, err := json.Marshal(fn)
		fmt.Printf("RESULT: %s,%v\n", b, err)
	}
	{
		var fn Func
		if err := Extract(f, "Sum2", &fn); err != nil {
			return fmt.Errorf("Sum2, %w", err)
		}
		b, err := json.Marshal(fn)
		fmt.Printf("RESULT: %s,%v\n", b, err)
	}
	{
		var fn Func
		if err := Extract(f, "IncAll", &fn); err != nil {
			return fmt.Errorf("IncAll, %w", err)
		}
		b, err := json.Marshal(fn)
		fmt.Printf("RESULT: %s,%v\n", b, err)
	}
	return nil
}
