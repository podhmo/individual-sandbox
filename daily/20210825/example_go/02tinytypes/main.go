package main

import (
	"bytes"
	"fmt"
	"go/types"
)

func main() {
	// use go/type
	u := NewUniverse()
	{
		xxx := u.NewPackage("m/xxx", "xxx")
		main := u.NewPackage("main", "main")

		params := types.NewTuple(
			xxx.NewVar("foo", xxx.NewNamed("Foo")),
		)
		returns := types.NewTuple()
		foo := types.NewSignature(nil, params, returns, false)

		fmt.Println(foo)

		var buf bytes.Buffer
		// types.WriteSignature(&buf, foo, types.RelativeTo(xxx))
		// fmt.Fprintln(&buf, "")
		// types.WriteSignature(&buf, foo, types.RelativeTo(main))
		types.WriteSignature(&buf, foo, Imported(main.Package))
		fmt.Fprintln(&buf, "")
		types.WriteSignature(&buf, foo, Imported(xxx.Package))
		fmt.Println(buf.String())
	}
	{
		fmt.Println(types.Universe.Lookup("string"))
	}
}

func NewUniverse() *Universe {
	return &Universe{
		Packages: map[string]*Package{},
	}
}

type Universe struct {
	Packages map[string]*Package
}

func (u *Universe) NewPackage(path, name string) *Package {
	pkg, ok := u.Packages[path]
	if ok {
		return pkg
	}
	pkg = &Package{
		Package: types.NewPackage(path, name),
	}
	u.Packages[path] = pkg
	return pkg
}

func Imported(here *types.Package) types.Qualifier {
	return func(there *types.Package) string {
		if here == there {
			return ""
		}
		return there.Name()
	}
}

type Package struct {
	*types.Package
	// todo: store
}

func (p *Package) NewVar(name string, typ types.Type) *types.Var {
	return types.NewVar(0, p.Package, name, typ)
}
func (p *Package) NewNamed(name string) *types.Named {
	return types.NewNamed(
		types.NewTypeName(0, p.Package, name, nil),
		nil, nil,
	)
}

// func (p *Package) NewSignature(receiver *types.Type) *types.Signature {
// 	return types.NewSignature(nil, params, returns, false)
// }
// func (p *Package) NewFunc(name string, sig *types.Signature) {
// 	return &Func{
// 		Func: types.NewFunc(0, p.Package, name, si),
// 	}
// }

type Func struct {
	*types.Func
}
