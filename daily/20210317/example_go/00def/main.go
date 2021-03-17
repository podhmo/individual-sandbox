package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"sort"
)

func main() {
	c := DefaultConfig

	helloPkg := c.NewPackage(Name("hello"), "m/hello")
	defaultName := helloPkg.NewVariable(
		"DefaultName", nil,
		"Foo",
	)
	hello := helloPkg.NewFunction(
		"Hello", nil,
		func(env Env, w io.Writer) error {
			_, err := io.WriteString(w, `func Hello(name string) string {
	return fmt.Sprintf("Hello %s", name)l
}`)
			return err
		},
	)

	fmt.Println(helloPkg)
	fmt.Println(hello)
	fmt.Println(defaultName)
	fmt.Println("----------------------------------------")
	helloPkg.Describe(os.Stdout)
}

type Config struct {
	SubpartPrefix string
	StrictMode    bool
}

type Name string

func (c *Config) NewPackage(name Name, path string) *Package {
	return &Package{
		config:        c,
		Name:          name,
		Path:          path,
		SymbolTable:   map[string]*Symbol{},
		FunctionTable: map[string]*Function{},
		VariableTable: map[string]*Variable{},
	}
}

type Package struct {
	config   *Config
	nameless bool

	Name Name
	Path string

	SymbolTable   map[string]*Symbol
	FunctionTable map[string]*Function
	VariableTable map[string]*Variable
}

func (p *Package) String() string {
	return fmt.Sprintf("<pkg %s>", p.Path)
}

func (p *Package) NewSymbol(name string, typ *Type) *Symbol {
	// TODO: mutex?
	sym, cached := p.SymbolTable[name]
	if cached {
		if typ == nil || sym.Type == typ {
			return sym
		}
		// handle DEBUG env?
		log.Printf("in %s, overwrite symbol  %s -> %s", p, sym.Type, typ)
	}
	sym = &Symbol{
		Name:    Name(name),
		Package: p,
		Type:    typ,
	}
	p.SymbolTable[name] = sym
	return sym
}

func (p *Package) LookupSymbol(name string) *Symbol {
	sym, ok := p.SymbolTable[name]
	if !ok && p.config.StrictMode {
		panic(fmt.Sprintf("symbol %s is not found in %s", name, p))
	}
	return sym
}

func (p *Package) NewType(name string, kind Kind) *Type {
	return &Type{
		Name:    Name(name),
		Package: p,
		Kind:    kind,
	}
}

func (p *Package) NewFunction(name string, typ *Type, emit EmitFunc) *Function {
	// TODO: mutex?
	fn, cached := p.FunctionTable[name]
	if cached {
		// handle DEBUG env?
		log.Printf("in %s, overwrite function %s -> ?", p, fn)
	}
	fn = &Function{
		Symbol:  p.NewSymbol(name, typ),
		Emit:    emit,
		Emitted: false,
	}
	p.FunctionTable[name] = fn
	return fn
}
func (p *Package) NewVariable(name string, typ *Type, value interface{}) *Variable {
	// TODO: mutex?
	fn, cached := p.VariableTable[name]
	if cached {
		// handle DEBUG env?
		log.Printf("in %s, overwrite function %s -> ?", p, fn)
	}
	fn = &Variable{
		Symbol:  p.NewSymbol(name, typ),
		Value:   value,
		Emitted: false,
	}
	p.VariableTable[name] = fn
	return fn
}

func (p *Package) Describe(w io.Writer) {
	fmt.Fprintf(w, "%s\n", p)

	used := map[Name]bool{}

	{
		names := make([]string, len(p.FunctionTable))
		{
			i := 0
			for name := range p.FunctionTable {
				names[i] = name
				i++
			}
			sort.Strings(names)
		}
		for _, name := range names {
			fn := p.FunctionTable[name]
			used[fn.Symbol.Name] = true
			fmt.Fprintf(w, "%s\n", fn)
		}
	}

	{
		names := make([]string, len(p.VariableTable))
		{
			i := 0
			for name := range p.VariableTable {
				names[i] = name
				i++
			}
			sort.Strings(names)
		}
		for _, name := range names {
			fn := p.VariableTable[name]
			used[fn.Symbol.Name] = true
			fmt.Fprintf(w, "%s\n", fn)
		}
	}

	{
		names := make([]string, len(p.SymbolTable))
		{
			i := 0
			for name := range p.SymbolTable {
				names[i] = name
				i++
			}
			sort.Strings(names)
		}
		for _, name := range names {
			sym := p.SymbolTable[name]
			if _, ok := used[sym.Name]; ok {
				continue
			}
			fmt.Fprintf(w, "%s\n", sym)
			used[sym.Name] = true
		}
	}
}

type Symbol struct {
	Name    Name
	Package *Package
	Type    *Type
}

func (s *Symbol) String() string {
	return fmt.Sprintf("<symbol %s.%s>", s.Package.Path, s.Name)
}

type EmitFunc func(env Env, w io.Writer) error
type Function struct {
	Symbol  *Symbol
	Emit    EmitFunc
	Emitted bool
}

func (f *Function) String() string {
	c := f.Symbol.Package.config
	return fmt.Sprintf("%s<function %s (emitted=%v)>", c.SubpartPrefix, f.Symbol, f.Emitted)
}

type Variable struct {
	Symbol  *Symbol
	Value   interface{}
	Emitted bool
}

func (f *Variable) String() string {
	c := f.Symbol.Package.config
	return fmt.Sprintf("%s<variable %s (emitted=%v)>", c.SubpartPrefix, f.Symbol, f.Emitted)
}

// 自信はない
type Env map[string]Symbol

// compositeだとか色々ありそう？ (underling, string)
// TODO:
type Type struct {
	Name    Name
	Package *Package
	Kind    Kind
}

type Kind = string

var Universe *Package
var DefaultConfig = &Config{
	SubpartPrefix: "  ",
	StrictMode:    true,
}

func generateUniverse(c *Config) *Package {
	pkg := c.NewPackage("butilins", "")
	pkg.nameless = true

	pkg.NewSymbol("int", pkg.NewType("int", Kind("int")))
	pkg.NewSymbol("int32", pkg.NewType("int32", Kind("int32")))
	pkg.NewSymbol("int64", pkg.NewType("int64", Kind("int64")))
	pkg.NewSymbol("string", pkg.NewType("string", Kind("string")))
	return pkg
}
func init() {
	Universe = generateUniverse(DefaultConfig)
}
