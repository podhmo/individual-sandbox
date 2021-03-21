package main

import (
	"fmt"
	"io"
	"strings"
)

func main() {
	mainPkg := &Package{Name: "main"}
	fooPkg := &Package{Name: "foo"}

	show := func(typ Type) {
		t := &ImportedType{Package: mainPkg, Type: typ}
		fmt.Printf("	%-30s %v\n", "use as name:", t)
		fmt.Printf("	%-30s %+v\n", "use as name with prefix:", t)
	}

	{
		t := &Named{Name: "string"}
		show(t)
	}
	fmt.Println("type X string")
	{
		t := &Named{Name: "X", Info: &Info{Underlying: &Named{Name: "string"}}}
		show(t)
	}
	fmt.Println("type Y string")
	{
		t := &Named{Name: "Y", Info: &Info{Package: fooPkg, Underlying: &Named{Name: "string"}}}
		show(t)
	}
	fmt.Println("type Z string (in package foo)")
	{
		pkg := &Package{Name: "foo", Path: "m/foo"}
		t := &Named{Name: "Foo", Info: &Info{Package: pkg, Underlying: &Named{Name: "string"}}}
		show(t)
	}
	fmt.Println("[]Z (in package foo)")
	{
		pkg := &Package{Name: "foo", Path: "m/foo"}
		t := &Named{Name: "Foo", Info: &Info{Package: pkg, Underlying: &Named{Name: "string"}}}
		show(t)
	}
	// fmt.Println("type ZList (in package foo)")
	// {
	// 	pkg := &Package{Name: "foo", Path: "m/foo"}
	// 	t := &Named{Name: "Foo", Info: &Info{Package: pkg, Underlying: &Named{Name: "string"}}}
	// 	show(t)
	// }
}

type Package struct {
	Name string
	Path string
}

// ----------------------------------------
// type implementation
// ----------------------------------------
type ImportedType struct {
	Package *Package
	Type
}

func (t *ImportedType) Format(f fmt.State, verb rune) {
	flags := make([]int, 0, 4)
	if f.Flag('+') {
		flags = append(flags, '+')
	}
	io.WriteString(f, t.Type.TypeString(t.Package, flags))
}

type Type interface {
	TypeString(here *Package, flags []int) string
	info() *Info
}

type Info struct {
	Package    *Package
	Lv         int
	Underlying Type
}

func (i *Info) info() *Info {
	return i
}

type Named struct {
	Name string
	*Info
}

func (t *Named) TypeString(here *Package, flags []int) string {
	withPlus := false
	for _, c := range flags {
		if c == '+' {
			withPlus = true
		}
	}

	prefix := ""
	if t.Info != nil && t.Info.Lv > 0 {
		prefix = strings.Repeat("*", t.Info.Lv)
	}
	if withPlus && t.Info != nil && t.Package != nil && t.Info.Package != here {
		return fmt.Sprintf("%s%s.%s", prefix, t.Info.Package.Name, t.Name)
	} else {
		return fmt.Sprintf("%s%s", prefix, t.Name)
	}
}

// for generics
type Container struct {
	*Info
	Name string
	Args []Type `json:"args"`
}

func (t *Container) TypeString(here *Package, flags []int) string {
	withPlus := false
	for _, c := range flags {
		if c == '+' {
			withPlus = true
		}
	}

	prefix := ""
	if t.Info != nil && t.Info.Lv > 0 {
		prefix = strings.Repeat("*", t.Info.Lv)
	}
	if withPlus && t.Info != nil && t.Package != nil && t.Info.Package != here {
		return fmt.Sprintf("%s%s.%s", prefix, t.Info.Package.Name, t.Name)
	} else {
		return fmt.Sprintf("%s%s", prefix, t.Name)
	}
}

// type Params struct {
// 	Keys   []string `json:"keys"`
// 	Values []Type   `json:"values"`
// }

// func (m *Params) Len() int {
// 	return len(m.Keys)
// }

// type Primitive struct {
// 	*Info
// }

// func (v *Primitive) Format(f fmt.State, verv rune) {
// 	prefix := ""
// 	if v.Lv > 0 {
// 		prefix = strings.Repeat("*", v.Lv)
// 	}
// 	fmt.Fprintf(f, "%s%s", prefix, v.Name)
// }

// type Function struct {
// 	*Info
// 	Params  Params `json:"params"`  // for function's In
// 	Returns Params `json:"returns"` // for function's Out
// }

// func (v *Function) Format(f fmt.State, c rune) {
// 	expr := "%"
// 	if f.Flag('#') {
// 		expr += "#"
// 	}
// 	if f.Flag('+') {
// 		expr += "+"
// 	}
// 	expr += string(c)

// 	params := make([]string, len(v.Params.Keys))
// 	for i, val := range v.Params.Values {
// 		params[i] = fmt.Sprintf(expr, val)
// 	}
// 	returns := make([]string, len(v.Returns.Keys))
// 	for i, val := range v.Returns.Values {
// 		returns[i] = fmt.Sprintf(expr, val)
// 	}
// 	switch len(returns) {
// 	case 0:
// 		fmt.Fprintf(f, "func(%s)",
// 			strings.Join(params, ", "),
// 		)
// 	case 1:
// 		fmt.Fprintf(f, "func(%s) %s",
// 			strings.Join(params, ", "),
// 			returns[0],
// 		)
// 	default:
// 		fmt.Fprintf(f, "func(%s) (%s)",
// 			strings.Join(params, ", "),
// 			strings.Join(returns, ", "),
// 		)
// 	}
// }
