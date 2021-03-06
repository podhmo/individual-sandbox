package main

import (
	"fmt"
	"go/types"
	"log"
	"reflect"

	"github.com/pkg/errors"
	"github.com/podhmo/handwriting"
	"github.com/podhmo/handwriting/multifile"
)

// Scan :
func Scan(pkg *types.Package) map[types.Type][]types.Object {
	r := map[types.Type][]types.Object{}
	s := pkg.Scope()
	for _, name := range reflect.ValueOf(s).Elem().FieldByName("elems").MapKeys() {
		ob := s.Lookup(name.String())
		r[ob.Type()] = append(r[ob.Type()], ob)
	}
	return r
}

func run(dir, name, filename string) error {
	planner, err := handwriting.NewFromPackagePath(dir, handwriting.WithOpener(multifile.Must(multifile.Dir(dir))))
	if err != nil {
		return err
	}
	f := planner.File(filename)

	f.Header(fmt.Sprintf(`// Code generated by "stringer -type %s"; DO NOT EDIT.`, name))

	f.Import("fmt")
	f.Code(func(e *handwriting.Emitter) error {
		pkg := e.PkgInfo.Pkg
		o := e.Output

		target := pkg.Scope().Lookup(name)
		if target == nil {
			return errors.Errorf("%q is not found from package %q", name, pkg.Path())
		}

		// todo : reuse
		typeMap := Scan(pkg)

		o.Println("// String :")
		o.WithBlock(fmt.Sprintf("func (x %s) String() string", target.Name()), func() {
			o.Println("switch x {")
			switch types.Identical(target.Type().Underlying(), types.Universe.Lookup("string").Type()) {
			case true:
				for _, ob := range typeMap[target.Type()] {
					if ob, ok := ob.(*types.Const); ok {
						o.WithIndent(fmt.Sprintf("case %s:", ob.Name()), func() {
							o.Println(fmt.Sprintf("return %s", ob.Val()))
						})
					}
				}
				o.WithIndent("default:", func() {
					o.Printfln(`return fmt.Sprintf("%s(%%q)", string(x))`, target.Name())
				})
			default:
				for _, ob := range typeMap[target.Type()] {
					if ob, ok := ob.(*types.Const); ok {
						o.WithIndent(fmt.Sprintf("case %s:", ob.Name()), func() {
							o.Println(fmt.Sprintf("return %q", ob.Name()))
						})
					}
				}
				o.WithIndent("default:", func() {
					o.Printfln(`return fmt.Sprintf("%s(%%q)", x)`, target.Name())
				})
			}
			o.Println("}")
		})
		return nil
	})
	return planner.Emit()
}

func main() {
	dir := "../00foo"
	typename := "Foo"
	filename := "foo_string.go"

	if err := run(dir, typename, filename); err != nil {
		log.Fatalf("%+v", err)
	}
}
