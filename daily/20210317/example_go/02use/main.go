package main

import (
	"fmt"
	"io"
)

func main() {
	mainPkg := &Package{Name: "main"}
	fooPkg := &ImportedPackage{
		Here: mainPkg,
		Package: &Package{
			Name: "foo",
			Path: "m/foo",
		},
	}

	// defined by foo
	helloFn := &ImportedSymbol{
		ImportedPackage: fooPkg,
		Symbol: &Symbol{
			Name:      "Hello",
			Type:      "func (string) string",
			IsLiteral: true,
			Package:   fooPkg.Package,
		},
	}
	hello2Fn := &ImportedSymbol{
		ImportedPackage: fooPkg,
		Symbol: &Symbol{
			Name:    "Hello2",
			Type:    "MessageFunc",
			Package: fooPkg.Package,
		},
	}

	// defined by main
	byebyeFn := &ImportedSymbol{
		Symbol: &Symbol{
			Name:      "Byebye",
			Type:      "func (string) string",
			IsLiteral: true,
			Package:   fooPkg.Package,
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
	fmt.Println("}")
}

type Package struct {
	Name string
	Path string
}

type ImportedPackage struct {
	*Package
	Here *Package
	As   string
}

func (p *ImportedPackage) Format(f fmt.State, verb rune) {
	switch verb {
	case 'q':
		if p.As != p.Package.Name {
			fmt.Fprintf(f, "%s %q", p.As, p.Package.Path)
		} else if f.Flag('+') {
			fmt.Fprintf(f, "%s %q", p.Package.Name, p.Package.Path)
		} else {
			fmt.Fprintf(f, "%q", p.Package.Path)
		}
	case 't', 'v':
		if p.As != "" {
			io.WriteString(f, p.As)
		} else {
			io.WriteString(f, p.Package.Name)
		}
	default:
		io.WriteString(f, p.Package.Name) // TODO: fix
	}
}

type Symbol struct {
	Name      string
	Type      string
	IsLiteral bool
	Package   *Package
}

type ImportedSymbol struct {
	*Symbol
	*ImportedPackage
	// Level (for pointer)
}

func (s *ImportedSymbol) Format(f fmt.State, verb rune) {
	switch verb {
	case 'v':
		if f.Flag('+') && s.ImportedPackage != nil {
			// e.g. foo.Foo
			fmt.Fprintf(f, "%t.%s", s.ImportedPackage, s.Name)
		} else if f.Flag('#') {
			// e.g. foo m.String
			if s.ImportedPackage != nil && !s.Symbol.IsLiteral {
				fmt.Fprintf(f, "%s %t.%s", s.Name, s.ImportedPackage, s.Type)
			} else {
				fmt.Fprintf(f, "%s %s", s.Name, s.Type)
			}
		} else {
			// e.g. Foo
			io.WriteString(f, s.Name)
		}
	case 't':
		if f.Flag('+') && !s.Symbol.IsLiteral {
			// e.g. m.String
			fmt.Fprintf(f, "%t.%s", s.ImportedPackage, s.Type)
		} else {
			// e.g. String
			io.WriteString(f, s.Type)
		}
	default:
		io.WriteString(f, s.Symbol.Name)
	}
}
