package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
	"os"
)

// TODO: extract struct information
// TODO: support iota
// TODO: tags extraction
// TODO: comment extraction

var target = flag.String("target", "", "target")

func main() {
	flag.Parse()
	args := flag.Args()
	_ = args
	if *target == "" {
		fmt.Fprintf(os.Stderr, "go run main.go --target [target]\n")
		os.Exit(1)
	}

	fpath := *target
	pkgs, e := CollectPackageMap(fpath)
	if e != nil {
		log.Fatal(e)
		return
	}

	world := NewWorld()
	for _, pkg := range pkgs {
		module := NewModule(pkg.Name)
		world.Modules[pkg.Name] = module
		for fname, f := range pkg.Files {
			result, err := CollectResult(fname, f.Scope)
			if err != nil {
				panic(err)
			}
			module.Files[fname] = result
		}
	}
	b, err := json.Marshal(world)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(b))
}

func CollectPackageMap(fpath string) (map[string]*ast.Package, error) {
	stat, err := os.Stat(fpath)
	if err != nil {
		return nil, err
	}
	fset := token.NewFileSet()
	if stat.IsDir() {
		return parser.ParseDir(fset, fpath, nil, 0)
	}
	f, err := parser.ParseFile(fset, fpath, nil, 0)
	if err != nil {
		return nil, err
	}
	pkg := &ast.Package{
		Name:  f.Name.Name,
		Files: make(map[string]*ast.File),
	}
	pkg.Files[fpath] = f

	m := make(map[string]*ast.Package)
	m[fpath] = pkg
	return m, nil
}

type World struct {
	Modules map[string]*Module `json:"module"`
}
type Module struct {
	Name  string             `json:"name"`
	Files map[string]*Result `json:"file"`
}

func NewWorld() *World {
	return &World{Modules: make(map[string]*Module)}
}
func NewModule(name string) *Module {
	return &Module{Name: name, Files: make(map[string]*Result)}
}

// Kind : kind of detectResult
type Kind string

// kind candidates
const (
	KindAlias           = Kind("alias")
	KindAliasCandidates = Kind("alias-candidates")
	KindStruct          = Kind("struct")
)

type Result struct {
	Name            string                        `json:"name"`
	NewTypeMap      map[string]*NewTypeDefinition `json:"newtype"`
	StructMap       map[string]*StructDefinition  `json:"struct"`
	MaybeNewTypeses []*NewTypeValue               `json:"-"`
}

func NewResult(name string) *Result {
	return &Result{
		Name:            name,
		StructMap:       make(map[string]*StructDefinition),
		NewTypeMap:      make(map[string]*NewTypeDefinition),
		MaybeNewTypeses: []*NewTypeValue{},
	}
}

func (r *Result) AddStruct(ob *ast.Object) (*StructDefinition, error) {
	item, exists := r.StructMap[ob.Name]
	if !exists {
		item = &StructDefinition{}
	}
	item.rawDef = ob
	item.Name = ob.Name
	r.StructMap[ob.Name] = item
	return item, nil
}

func (r *Result) AddNewType(ob *ast.Object) (*NewTypeDefinition, error) {
	item, exists := r.NewTypeMap[ob.Name]
	if !exists {
		item = &NewTypeDefinition{}
	}
	item.rawDef = ob
	item.Name = ob.Name
	r.NewTypeMap[ob.Name] = item
	if !exists {
		newGuesses := make([]*NewTypeValue, 0, len(r.MaybeNewTypeses))
		for _, value := range r.MaybeNewTypeses {
			// xxx:
			if ob.Name == value.TypeName {
				r.AddNewTypeValue(value.rawDef)
			} else {
				newGuesses = append(newGuesses, value)
			}
		}
		r.MaybeNewTypeses = newGuesses
	}
	return item, nil
}

type callexprVisitor struct {
	Found *ast.CallExpr
}

func (v *callexprVisitor) Visit(node ast.Node) ast.Visitor {
	if v.Found == nil && node != nil {
		if callexpr, ok := node.(*ast.CallExpr); ok {
			v.Found = callexpr
		}
	}
	return v
}

func findCallExpr(val ast.Node) (*ast.CallExpr, error) {
	v := &callexprVisitor{}
	ast.Walk(v, val)
	if v.Found == nil {
		return nil, fmt.Errorf("callExpr is not found")
	}
	return v.Found, nil
}

type basicLitVisitor struct {
	Found *ast.BasicLit
}

func (v *basicLitVisitor) Visit(node ast.Node) ast.Visitor {
	if v.Found == nil && node != nil {
		if basicLit, ok := node.(*ast.BasicLit); ok {
			v.Found = basicLit
		}
	}
	return v
}

func findBasicLit(val ast.Node) (*ast.BasicLit, error) {
	v := &basicLitVisitor{}
	ast.Walk(v, val)
	if v.Found == nil {
		return nil, fmt.Errorf("basicLit is not found")
	}
	return v.Found, nil
}

func findValueType(val ast.Node) (*ast.CallExpr, error) {
	v := &callexprVisitor{}
	ast.Walk(v, val)
	if v.Found == nil {
		return nil, fmt.Errorf("callExpr is not found")
	}
	return v.Found, nil
}

func (r *Result) AddNewTypeValue(ob *ast.Object) (*NewTypeDefinition, error) {
	spec := ob.Decl.(*ast.ValueSpec)
	values := spec.Values
	if len(values) != 1 {
		return nil, fmt.Errorf("hmm:%v", values) // xxx;
	}
	var value *NewTypeValue
	switch node := spec.Type.(type) {
	case *ast.Ident:
		typeName := node.Name
		for _, v := range values {
			lit, err := findBasicLit(v)
			if err != nil {
				return nil, err // xxx;
			}
			if lit == nil {
				return nil, fmt.Errorf("not found: %s", ob.Name)
			}
			value = &NewTypeValue{TypeName: typeName, Name: ob.Name, Value: lit.Value, rawDef: ob}
			break
		}
	case nil:
		// finding type name
		for _, v := range values {
			f, err := findCallExpr(v)
			if err != nil {
				return nil, err // xxx;
			}
			if f == nil {
				return nil, fmt.Errorf("not found: %s", ob.Name)
			}
			lit, err := findBasicLit(f)
			if err != nil {
				return nil, err // xxx;
			}
			if lit == nil {
				return nil, fmt.Errorf("not found: %s", ob.Name)
			}
			fident := f.Fun.(*ast.Ident)
			value = &NewTypeValue{TypeName: fident.Name, Name: ob.Name, Value: lit.Value, rawDef: ob}
			break
		}
	}
	// fident.Obj is same as type def?
	if _, ok := r.NewTypeMap[value.TypeName]; !ok {
		r.MaybeNewTypeses = append(r.MaybeNewTypeses, value)
		return nil, nil
	}

	item, exists := r.NewTypeMap[value.TypeName]
	if !exists {
		item = &NewTypeDefinition{}
		r.NewTypeMap[ob.Name] = item
	}
	item.Candidates = append(item.Candidates, value)
	item.rawCandidates = append(item.rawCandidates, ob)
	return item, nil
}

type StructDefinition struct {
	Name   string `json:"name"`
	rawDef *ast.Object
}

type NewTypeDefinition struct {
	Name          string          `json:"name"`
	Candidates    []*NewTypeValue `json:"candidates"`
	rawDef        *ast.Object
	rawCandidates []*ast.Object
}

type NewTypeValue struct {
	TypeName string      `json:"-"`
	Name     string      `json:"name"`
	Value    interface{} `json:"value"`
	rawDef   *ast.Object
}

func CollectResult(name string, scope *ast.Scope) (*Result, error) {
	r := NewResult(name)
	for _, ob := range scope.Objects {
		anyFound := false
		if isStructDefinition(ob) {
			anyFound = true
			_, err := r.AddStruct(ob)
			if err != nil {
				return r, err
			}
		}
		if isNewTypeDefinition(ob) {
			anyFound = true
			_, err := r.AddNewType(ob)
			if err != nil {
				return r, err
			}

		}
		if isNewTypeValueDefinition(ob) {
			anyFound = true
			_, err := r.AddNewTypeValue(ob)
			if err != nil {
				return r, err
			}
		}
		if !anyFound {
			fmt.Println(ob.Name)
		}
	}
	return r, nil
}

func isStructDefinition(ob *ast.Object) bool {
	if ob.Kind != ast.Typ {
		return false
	}

	node, ok := ob.Decl.(*ast.TypeSpec)
	if !ok {
		return false
	}

	_, ok = node.Type.(*ast.StructType)
	if !ok {
		return false
	}
	return true
}

func isNewTypeDefinition(ob *ast.Object) bool {
	if ob.Kind != ast.Typ {
		return false
	}
	node, ok := ob.Decl.(*ast.TypeSpec)
	if !ok {
		return false
	}

	_, ok = node.Type.(*ast.Ident)
	if !ok {
		return false
	}
	return true
}

func isNewTypeValueDefinition(ob *ast.Object) bool {
	if ob.Kind != ast.Con {
		return false
	}
	_, ok := ob.Decl.(*ast.ValueSpec)
	if !ok {
		return false
	}
	return true
}
