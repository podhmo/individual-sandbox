package main

import (
	"encoding/json"
	"fmt"
	"go/token"
	"go/types"
	"log"
	"os"
	"strings"

	"github.com/k0kubun/pp"
	"github.com/pkg/errors"
	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(args []string) error {
	path := args[0]

	fset := token.NewFileSet()
	visitor := NewVisitor(fset, path)

	r := map[string]*Package{}
	err := visitor.VisitNamed(func(pkg *packages.Package, ob types.Object) {
		p := r[pkg.ID]
		if p == nil {
			p = NewPackage(pkg)
			r[pkg.ID] = p
		}

		switch typ := ob.Type().(type) {
		case *types.Named:
			switch underlying := typ.Underlying().(type) {
			case *types.Struct:
				s := Struct{
					Name: ob.Id(),
					Raw:  ob,
				}
				underlying, isStruct := ob.Type().Underlying().(*types.Struct)
				if !isStruct {
					return
				}
				for i := 0; i < underlying.NumFields(); i++ {
					f := underlying.Field(i)
					// todo: embeded
					s.Fields = append(s.Fields, Field{
						Name: f.Name(),
						Type: AsType(f.Type()), // xxx
						Tag:  underlying.Tag(i),
						Raw:  f,
					})
				}
				p.Structs = append(p.Structs, s)
				fmt.Printf("%T %s\n", ob.Type(), underlying)
			case *types.Basic:
				p.NewTypes = append(p.NewTypes, NewType{
					Name: ob.Id(),
					Type: AsType(underlying),
					Raw:  ob,
				})
			default:
				fmt.Println("@1", ob.Type(), underlying)
				pp.Println(underlying)
			}
		default:
			fmt.Println("@0", ob.Type())
		}
	})
	if err != nil {
		return err
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(r)
}

// Package :
type Package struct {
	Name     string           `json:"name"`
	Path     string           `json:"path"`
	Structs  []Struct         `json:"structs"`
	NewTypes []NewType        `json:"newTypes"`
	Imported []PackageSummary `json:"imported"` // todo: qualified pkgname

	Raw *packages.Package `json:"-"`
}

// NewPackage :
func NewPackage(pkg *packages.Package) *Package {
	imported := make([]PackageSummary, 0, len(pkg.Imports))
	for _, impkg := range pkg.Imports {
		imported = append(imported, PackageSummary{
			Name: impkg.Name,
			Path: impkg.PkgPath,
			Raw:  impkg,
		})
	}
	p := &Package{
		Name:     pkg.Name,
		Path:     pkg.PkgPath,
		Imported: imported,
		NewTypes: []NewType{},
		Raw:      pkg,
	}
	return p
}

// PackageSummary :
type PackageSummary struct {
	Name string `json:"name"`
	Path string `json:"path"`

	Raw *packages.Package `json:"-"`
}

// Struct :
type Struct struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`

	Raw types.Object `json:"-"`
}

// Field :
type Field struct {
	Name string `json:"name"`
	Tag  string `json:"tag,omitempty"`
	Type Type   `json:"type"` // ?

	Raw *types.Var `json:"-"`
}

// NewType :
type NewType struct {
	Name string `json:"name"`
	Type Type   `json:"type"`

	Raw types.Object `json:"-"`
}

// Type :
type Type struct {
	Ident string `json:"ident"`
	Level int    `json:"level,omitempty"`
	Args  []Type `json:"args,omitempty"`

	Raw types.Type `json:"-"`
}

// Visitor :
type Visitor struct {
	fset *token.FileSet
	path string
	syms []string
	mode packages.LoadMode
}

// NewVisitor
func NewVisitor(
	fset *token.FileSet,
	path string,
) *Visitor {
	var syms []string
	if strings.Contains(path, ":") {
		splited := strings.SplitN(path, ":", 2)
		path = splited[0]
		syms = append(syms, splited[1])
	}
	return &Visitor{
		fset: fset,
		path: path,
		syms: syms,
		mode: packages.NeedName | packages.NeedImports | packages.NeedTypes,
	}
}

// VisitNamed :
func (v *Visitor) VisitNamed(visit func(pkg *packages.Package, ob types.Object)) error {
	cfg := &packages.Config{
		Fset: v.fset,
		Mode: v.mode,
	}
	pkgs, err := packages.Load(cfg, v.path)
	if err != nil {
		return errors.Wrap(err, "load")
	}
	if len(pkgs) != 1 {
		return errors.Wrap(fmt.Errorf("something wrong: must be len(pkgs) == 1, but %d", len(pkgs)), "not found")
	}

	pkg := pkgs[0] // using packages.Visit()?
	s := pkg.Types.Scope()
	for _, name := range s.Names() {
		ob := s.Lookup(name)
		if !ob.Exported() {
			continue
		}
		found := true
		name := ob.Id()
		if len(v.syms) > 0 {
			found = false
			for _, sym := range v.syms {
				if name == sym {
					found = true
					break
				}
			}
		}
		if !found {
			continue
		}
		visit(pkg, ob)
	}
	return nil
}

// AsType :
func AsType(typ types.Type) Type {
	return asType(typ, 0, make([]types.Type, 0, 8))
}

func asType(typ types.Type, level int, visited []types.Type) Type {
	// Theoretically, this is a quadratic lookup algorithm, but in
	// practice deeply nested composite types with unnamed component
	// types are uncommon. This code is likely more efficient than
	// using a map.
	for _, t := range visited {
		if t == typ {
			return Type{
				Ident:  fmt.Sprintf("○%T", typ), // cycle to typ
				Raw:   typ,
				Level: level,
			}
		}
	}
	visited = append(visited, typ)

	switch t := typ.(type) {
	case nil:
		return Type{
			Ident: "nil",
			Level: level,
		}
	case *types.Basic:
		return Type{
			Ident: t.Name(),
			Raw:   typ,
			Level: level,
		}

	// case *types.Array:
	// 	fmt.Fprintf(buf, "[%d]", t.len)
	// 	writeType(buf, t.elem, qf, visited)

	case *types.Slice:
		return Type{
			Ident: "Slice",
			Raw:   typ,
			Level: level,
			Args:  []Type{AsType(t.Elem())},
		}

	// case *types.Struct:
	// 	buf.WriteString("struct{")
	// 	for i, f := range t.fields {
	// 		if i > 0 {
	// 			buf.WriteString("; ")
	// 		}
	// 		if !f.embedded {
	// 			buf.WriteString(f.name)
	// 			buf.WriteByte(' ')
	// 		}
	// 		writeType(buf, f.typ, qf, visited)
	// 		if tag := t.Tag(i); tag != "" {
	// 			fmt.Fprintf(buf, " %q", tag)
	// 		}
	// 	}
	// 	buf.WriteByte('}')

	case *types.Pointer:
		return asType(t.Elem(), level+1, visited)

	// case *types.Tuple:
	// 	writeTuple(buf, t, false, qf, visited)

	// case *types.Signature:
	// 	buf.WriteString("func")
	// 	writeSignature(buf, t, qf, visited)

	// case *types.Interface:
	// 	// We write the source-level methods and embedded types rather
	// 	// than the actual method set since resolved method signatures
	// 	// may have non-printable cycles if parameters have embedded
	// 	// interface types that (directly or indirectly) embed the
	// 	// current interface. For instance, consider the result type
	// 	// of m:
	// 	//
	// 	//     type T interface{
	// 	//         m() interface{ T }
	// 	//     }
	// 	//
	// 	buf.WriteString("interface{")
	// 	empty := true
	// 	if gcCompatibilityMode {
	// 		// print flattened interface
	// 		// (useful to compare against gc-generated interfaces)
	// 		for i, m := range t.allMethods {
	// 			if i > 0 {
	// 				buf.WriteString("; ")
	// 			}
	// 			buf.WriteString(m.name)
	// 			writeSignature(buf, m.typ.(*Signature), qf, visited)
	// 			empty = false
	// 		}
	// 	} else {
	// 		// print explicit interface methods and embedded types
	// 		for i, m := range t.methods {
	// 			if i > 0 {
	// 				buf.WriteString("; ")
	// 			}
	// 			buf.WriteString(m.name)
	// 			writeSignature(buf, m.typ.(*Signature), qf, visited)
	// 			empty = false
	// 		}
	// 		for i, typ := range t.embeddeds {
	// 			if i > 0 || len(t.methods) > 0 {
	// 				buf.WriteString("; ")
	// 			}
	// 			writeType(buf, typ, qf, visited)
	// 			empty = false
	// 		}
	// 	}
	// 	if t.allMethods == nil || len(t.methods) > len(t.allMethods) {
	// 		if !empty {
	// 			buf.WriteByte(' ')
	// 		}
	// 		buf.WriteString("/* incomplete */")
	// 	}
	// 	buf.WriteByte('}')

	// case *types.Map:
	// 	buf.WriteString("map[")
	// 	writeType(buf, t.key, qf, visited)
	// 	buf.WriteByte(']')
	// 	writeType(buf, t.elem, qf, visited)

	// case *types.Chan:
	// 	var s string
	// 	var parens bool
	// 	switch t.dir {
	// 	case SendRecv:
	// 		s = "chan "
	// 		// chan (<-chan T) requires parentheses
	// 		if c, _ := t.elem.(*Chan); c != nil && c.dir == RecvOnly {
	// 			parens = true
	// 		}
	// 	case SendOnly:
	// 		s = "chan<- "
	// 	case RecvOnly:
	// 		s = "<-chan "
	// 	default:
	// 		panic("unreachable")
	// 	}
	// 	buf.WriteString(s)
	// 	if parens {
	// 		buf.WriteByte('(')
	// 	}
	// 	writeType(buf, t.elem, qf, visited)
	// 	if parens {
	// 		buf.WriteByte(')')
	// 	}

	case *types.Named:
		return Type{
			Ident: typ.String(),
			Raw:   typ,
			Level: level,
		}
	default:
		// For externally defined implementations of Type.
		return Type{
			Ident: t.String(),
			Raw:   typ,
			Level: level,
		}
	}
}
