package main

import (
	"fmt"
	"go/types"
	"io"
)

// TODO:
// - remove ImportedPackage
// - add Here in ImportedSymbol
// - add AsMap in Here (shared)
// - use types.NewVar as symbol

func main() {
	mainPkg := types.NewPackage("main", "main")
	fooPkg := &ImportedPackage{
		Here:    mainPkg,
		Package: types.NewPackage("m/foo", "foo"),
	}

	// defined by foo
	helloFn := &ImportedSymbol{
		ImportedPackage: fooPkg,
		Symbol: &Symbol{
			Name: "Hello",
			Type: types.NewSignature(nil,
				types.NewTuple(types.NewParam(0, nil, "name", types.Typ[types.String])),
				types.NewTuple(types.NewParam(0, nil, "", types.Typ[types.String])),
				false,
			),
			Package: fooPkg.Package,
		},
	}
	hello2Fn := &ImportedSymbol{
		ImportedPackage: fooPkg,
		Symbol: &Symbol{
			Name: "Hello2",
			Type: types.NewNamed(types.NewTypeName(0, fooPkg.Package, "MessageFunc", types.NewSignature(nil,
				types.NewTuple(types.NewParam(0, nil, "name", types.Typ[types.String])),
				types.NewTuple(types.NewParam(0, nil, "", types.Typ[types.String])),
				false)),
				nil, nil,
			),
			Package: fooPkg.Package,
		},
	}

	// defined by main
	byebyeFn := &ImportedSymbol{
		Symbol: &Symbol{
			Name: "Byebye",
			Type: types.NewSignature(nil,
				types.NewTuple(types.NewVar(0, nil, "name", types.Typ[types.String])),
				types.NewTuple(types.NewVar(0, nil, "", types.Typ[types.String])),
				false,
			),
			Package: mainPkg,
		},
	}
	val := &ImportedSymbol{
		Symbol: &Symbol{
			Name: "val",
			Type: types.NewPointer(types.Typ[types.Int]),
		},
	}
	val2 := &ImportedSymbol{
		ImportedPackage: fooPkg, // xxx
		Symbol: &Symbol{
			Name: "val2",
			Type: types.NewMap(types.Typ[types.String], types.NewSlice(types.NewNamed(types.NewTypeName(0, fooPkg.Package, "MessageFunc", types.NewSignature(nil,
				types.NewTuple(types.NewParam(0, nil, "name", types.Typ[types.String])),
				types.NewTuple(types.NewParam(0, nil, "", types.Typ[types.String])),
				false)),
				nil, nil,
			))),
		},
	}

	fmt.Println("package main")
	fmt.Println("import (")
	fmt.Printf("	%q\n", fooPkg)
	fmt.Printf("	%+q\n", fooPkg)
	fmt.Println(")")
	fmt.Println("")
	fmt.Println("func main(){")
	fmt.Println("	// -- imported symbol --")
	fmt.Printf("	// %-30s %v\n", "use as name:", helloFn)
	fmt.Printf("	// %-30s %+v\n", "use as name with prefix:", helloFn)
	fmt.Printf("	// %-30s %#v\n", "use as name with type info:", helloFn)
	fmt.Printf("	// %-30s %t\n", "use as type:", helloFn)
	fmt.Printf("	// %-30s %+t\n", "use as type with prefix:", helloFn)
	fmt.Println("")
	fmt.Println("")
	fmt.Printf("	// %-30s %v\n", "use as name:", hello2Fn)
	fmt.Printf("	// %-30s %+v\n", "use as name with prefix:", hello2Fn)
	fmt.Printf("	// %-30s %#v\n", "use as name with type info:", hello2Fn)
	fmt.Printf("	// %-30s %t\n", "use as type:", hello2Fn)
	fmt.Printf("	// %-30s %+t\n", "use as type with prefix:", hello2Fn)
	fmt.Println("")
	fmt.Println("")
	fmt.Println("	// -- in same package --")
	fmt.Printf("	// %-30s %v\n", "use as name:", byebyeFn)
	fmt.Printf("	// %-30s %+v\n", "use as name with prefix:", byebyeFn)
	fmt.Printf("	// %-30s %#v\n", "use as name with type info:", byebyeFn)
	fmt.Printf("	// %-30s %t\n", "use as type:", byebyeFn)
	fmt.Printf("	// %-30s %+t\n", "use as type with prefix:", byebyeFn)
	fmt.Println("")
	fmt.Println("")
	fmt.Println("	// -- pointer --")
	fmt.Printf("	// %-30s %v\n", "use as name:", val)
	fmt.Printf("	// %-30s %+v\n", "use as name with prefix:", val)
	fmt.Printf("	// %-30s %#v\n", "use as name with type info:", val)
	fmt.Printf("	// %-30s %t\n", "use as type:", val)
	fmt.Printf("	// %-30s %+t\n", "use as type with prefix:", val)
	fmt.Println("")
	fmt.Println("")
	fmt.Println("	// -- container --")
	fmt.Printf("	// %-30s %v\n", "use as name:", val2)
	fmt.Printf("	// %-30s %+v\n", "use as name with prefix:", val2)
	fmt.Printf("	// %-30s %#v\n", "use as name with type info:", val2)
	fmt.Printf("	// %-30s %t\n", "use as type:", val2)
	fmt.Printf("	// %-30s %+t\n", "use as type with prefix:", val2)
	fmt.Println("}")
}

type ImportedPackage struct {
	*types.Package
	Here *types.Package
	As   string
}

func (p *ImportedPackage) Format(f fmt.State, verb rune) {
	switch verb {
	case 'q':
		if p.As != p.Package.Name() {
			fmt.Fprintf(f, "%s %q", p.As, p.Package.Path())
		} else if f.Flag('+') {
			fmt.Fprintf(f, "%s %q", p.Package.Name(), p.Package.Path())
		} else {
			fmt.Fprintf(f, "%q", p.Package.Path())
		}
	case 't', 'v':
		if p.As != "" {
			io.WriteString(f, p.As)
		} else {
			io.WriteString(f, p.Package.Name())
		}
	default:
		io.WriteString(f, p.Package.Name()) // TODO: fix
	}
}

type Symbol struct {
	Name    string
	Type    types.Type
	Package *types.Package
}

type ImportedSymbol struct {
	*Symbol          // required
	*ImportedPackage // unrequired
}

func (s *ImportedSymbol) Format(f fmt.State, verb rune) {
	switch verb {
	case 'v':
		if f.Flag('+') && s.ImportedPackage != nil {
			// e.g. foo.Foo
			fmt.Fprintf(f, "%t.%s", s.ImportedPackage, s.Name)
		} else if f.Flag('#') {
			if s.ImportedPackage != nil {
				// e.g. foo m.String
				fmt.Fprintf(f, "%s %s", s.Name, types.TypeString(s.Type, s.Qualifier))
			} else {
				fmt.Fprintf(f, "%s %s", s.Name, types.TypeString(s.Type, noprefix))
			}
		} else {
			// e.g. Foo
			io.WriteString(f, s.Name)
		}
	case 't':
		if f.Flag('+') && s.ImportedPackage != nil {
			// e.g. m.String
			fmt.Fprintf(f, "%s", types.TypeString(s.Type, s.Qualifier))
		} else {
			// e.g. String
			fmt.Fprintf(f, "%s", types.TypeString(s.Type, noprefix))
		}
	default:
		io.WriteString(f, s.Symbol.Name)
	}
}

func (s *ImportedSymbol) Qualifier(other *types.Package) string {
	if s.ImportedPackage == nil {
		return ""
	}
	if s.ImportedPackage.Here == other {
		return "" // same package; unqualified
	}
	if s.ImportedPackage.As != "" {
		return s.ImportedPackage.As
	}
	return other.Name()
}

func noprefix(other *types.Package) string {
	return ""
}
