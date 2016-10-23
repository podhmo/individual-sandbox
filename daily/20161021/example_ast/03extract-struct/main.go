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

	"github.com/davecgh/go-spew/spew"
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
	Name  string             `json:"-"`
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
	Name          string                       `json:"-"`
	AliasMap      map[string]*AliasDefinition  `json:"alias"`
	StructMap     map[string]*StructDefinition `json:"struct"`
	MaybeAliasses []*AliasValue                `json:"-"`
}

func NewResult(name string) *Result {
	return &Result{
		Name:          name,
		StructMap:     make(map[string]*StructDefinition),
		AliasMap:      make(map[string]*AliasDefinition),
		MaybeAliasses: []*AliasValue{},
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
	fields, err := findFields(ob.Decl.(ast.Node))
	item.Fields = fields
	return item, err
}

type nameVisitor struct{ Found string }

func (v *nameVisitor) Visit(node ast.Node) ast.Visitor {
	switch node := node.(type) {
	case *ast.Ident:
		v.Found = node.Name
	}
	return v
}

func findName(node ast.Node) string {
	namevisitor := &nameVisitor{Found: ""}
	ast.Walk(namevisitor, node)
	return namevisitor.Found
}

type Field struct {
	Name  string   `json:"-"`
	Tags  []string `json:"tags"`
	Type  Type     `json:"type"`
	Embed bool     `json:"embed"`
}

type Type interface{}
type fieldsVisitor struct {
	Found map[string]*Field
}

func (v *fieldsVisitor) Visit(node ast.Node) ast.Visitor {
	structNode := node.(*ast.TypeSpec).Type.(*ast.StructType)
	if structNode.Incomplete {
		panic(fmt.Sprintf("%s is incomplete struct definition", node.(*ast.TypeSpec).Name.Name))
	}
	for _, field := range structNode.Fields.List {
		if err := v.visitField(field); err != nil {
			panic(err)
		}
	}
	return nil
}

func (v *fieldsVisitor) visitField(node *ast.Field) error {
	if len(node.Names) > 1 {
		return fmt.Errorf("%s has more than one length", node.Names)
	}
	typ := findType(node.Type)
	tags := []string{}
	if node.Tag != nil {
		tags = append(tags, node.Tag.Value)
	}
	var embed bool
	var name string
	if len(node.Names) == 0 {
		embed = true
		name = findName(node.Type)
	} else {
		embed = false
		name = node.Names[0].Name
	}

	v.Found[name] = &Field{
		Name:  name,
		Embed: embed,
		Tags:  tags,
		Type:  typ,
	}
	return nil
}

func findType(node ast.Node) Type {
	m := make(map[string]Type)
	switch node := node.(type) {
	case *ast.Ident:
		m["kind"] = "primitive"
		m["value"] = node.Name
	case *ast.ArrayType:
		m["kind"] = "array"
		m["value"] = findType(node.Elt)
	case *ast.MapType:
		m["kind"] = "map"
		m["key"] = findType(node.Key)
		m["value"] = findType(node.Value)
	case *ast.InterfaceType:
		m["kind"] = "interface"
		m["methods"] = findType(node.Methods)
	case *ast.StarExpr:
		m["kind"] = "pointer"
		m["value"] = findType(node.X)
	case *ast.SelectorExpr:
		m["kind"] = "selector"
		m["value"] = fmt.Sprintf("%s.%s", node.X.(*ast.Ident).Name, node.Sel)
	case *ast.FuncType:
		m["kind"] = "func"
		m["args"] = findType(node.Params)
		m["results"] = findType(node.Results)
	case *ast.TypeSpec:
		return findType(node.Type)
	case *ast.FieldList:
		args := make([]Type, len(node.List))
		for i, arg := range node.List {
			args[i] = findType(arg.Type)
		}
		return args
	default:
		spew.Dump(node)
		panic(node)
	}
	return m
}

func findFields(val ast.Node) (map[string]*Field, error) {
	v := &fieldsVisitor{Found: make(map[string]*Field)}
	ast.Walk(v, val)
	if v.Found == nil {
		return nil, fmt.Errorf("fields is not found")
	}
	return v.Found, nil
}

func (r *Result) AddAlias(ob *ast.Object) (*AliasDefinition, error) {
	item, exists := r.AliasMap[ob.Name]
	if !exists {
		item = &AliasDefinition{}
	}
	item.rawDef = ob
	item.Name = ob.Name
	if ob.Decl != nil {
		item.Original = findType(ob.Decl.(*ast.TypeSpec))
	}
	r.AliasMap[ob.Name] = item
	if !exists {
		newGuesses := make([]*AliasValue, 0, len(r.MaybeAliasses))
		for _, value := range r.MaybeAliasses {
			// xxx:
			if ob.Name == value.TypeName {
				r.AddAliasValue(value.rawDef)
			} else {
				newGuesses = append(newGuesses, value)
			}
		}
		r.MaybeAliasses = newGuesses
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

func (r *Result) AddAliasValue(ob *ast.Object) (*AliasDefinition, error) {
	spec := ob.Decl.(*ast.ValueSpec)
	values := spec.Values
	if len(values) != 1 {
		return nil, fmt.Errorf("hmm:%v", values) // xxx;
	}
	var value *AliasValue
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
			value = &AliasValue{TypeName: typeName, Name: ob.Name, Value: lit.Value, rawDef: ob}
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
				if _, ok := v.(*ast.BasicLit); ok {
					return nil, nil
				}
				fmt.Fprintf(os.Stderr, "const %s is complex definition. skip..\n", ob.Name)
				return nil, nil
			}
			lit, err := findBasicLit(f)
			if err != nil {
				return nil, err // xxx;
			}
			if lit == nil {
				return nil, fmt.Errorf("not found@@: %s", ob.Name)
			}
			fident := f.Fun.(*ast.Ident)
			value = &AliasValue{TypeName: fident.Name, Name: ob.Name, Value: lit.Value, rawDef: ob}
			break
		}
	}
	// fident.Obj is same as type def?
	if _, ok := r.AliasMap[value.TypeName]; !ok {
		r.MaybeAliasses = append(r.MaybeAliasses, value)
		return nil, nil
	}

	item, exists := r.AliasMap[value.TypeName]
	if !exists {
		item = &AliasDefinition{}
		r.AliasMap[ob.Name] = item
	}
	item.Candidates = append(item.Candidates, value)
	item.rawCandidates = append(item.rawCandidates, ob)
	return item, nil
}

type StructDefinition struct {
	Name   string `json:"-"`
	rawDef *ast.Object
	Fields map[string]*Field
}

type AliasDefinition struct {
	Name          string        `json:"-"`
	Original      Type          `json:"original"`
	Candidates    []*AliasValue `json:"candidates"`
	rawDef        *ast.Object
	rawCandidates []*ast.Object
}

type AliasValue struct {
	TypeName string      `json:"-"`
	Name     string      `json:"-"`
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
		if isAliasDefinition(ob) {
			anyFound = true
			_, err := r.AddAlias(ob)
			if err != nil {
				return r, err
			}

		}
		if isAliasValueDefinition(ob) {
			anyFound = true
			_, err := r.AddAliasValue(ob)
			if err != nil {
				return r, err
			}
		}
		if !anyFound {
			// fmt.Println(ob.Name)
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

func isAliasDefinition(ob *ast.Object) bool {
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

func isAliasValueDefinition(ob *ast.Object) bool {
	if ob.Kind != ast.Con {
		return false
	}
	_, ok := ob.Decl.(*ast.ValueSpec)
	if !ok {
		return false
	}
	return true
}
