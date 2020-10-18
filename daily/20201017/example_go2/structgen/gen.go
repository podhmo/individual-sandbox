package structgen

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"reflect"
	"strings"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

type Self struct {
}

var self = reflect.TypeOf(Self{})

type Visitor struct {
	Imports   []string
	BufferMap map[string]io.ReadWriter
	seen      []string
	OnlyOne   bool

	ReferenceMap map[reflect.Type]string
}

func (v *Visitor) Visit(s shape.Shape) error {
	return v.visit([]shape.Shape{s})

}
func (v *Visitor) visit(path []shape.Shape) error {
	s := path[len(path)-1]
	// if name, ok := v.ReferenceMap[s.GetReflectType()]; ok {
	// 	s.ResetName(name)
	// }
	k := s.GetFullName()
	// fmt.Fprintln(os.Stderr, "visit", s)

	w, visited := v.BufferMap[k]
	if visited {
		return nil
	}

	w = new(bytes.Buffer)
	v.BufferMap[k] = w
	v.seen = append(v.seen, k)

	// todo: import
	if s.GetPackage() != "" {
		v.Imports = append(v.Imports, s.GetPackage())
	}

	switch s := s.(type) {
	case shape.Struct:
		nl := []byte{'\n'}
		fmt.Fprintln(w, "// from", s.GetFullName())
		fmt.Fprintf(w, "type %s struct {\n", s.GetName())
		for i, name := range s.Fields.Keys {
			f := s.Fields.Values[i]

			// fmt.Fprintln(os.Stderr, "	!S", i, name, f.GetPackage(), "@", f)
			if !v.OnlyOne && (f.GetPackage() == path[0].GetPackage() || f.GetPackage() == "") {
				if err := v.visit(append(path, f)); err != nil {
					return err
				}
			} else {
				// 	fmt.Fprintln(os.Stderr, "		skip", i, name, s.GetPackage(), "!=", f.GetPackage())
			}

			if f.GetReflectType().AssignableTo(self) {
				f = s
			}
			if f.GetPackage() != "" {
				v.Imports = append(v.Imports, f.GetPackage())
			}

			fmt.Fprintf(w, "	%s %s", name, v.getTypeName(f))
			if string(s.Tags[i]) != "" {
				fmt.Fprintf(w, " `%s`", s.Tags[i])
			}
			w.Write(nl)
		}
		fmt.Fprintf(w, "}\n")
	case shape.Primitive:
		return nil
	case shape.Container:
		for _, f := range s.Args {
			// fmt.Fprintln(os.Stderr, "	!C", i, f.GetPackage(), "@", f)
			if !v.OnlyOne && (f.GetPackage() == path[0].GetPackage()) {
				if err := v.visit(append(path, f)); err != nil {
					return err
				}
			} else {
				//				fmt.Fprintln(os.Stderr, "		skip", i, s.GetPackage(), "!=", f.GetPackage())
			}

		}
	default:
		return fmt.Errorf("unsupported type %T", s)
	}
	return nil // never
}

func (v *Visitor) getTypeName(s shape.Shape) string {
	if name, ok := v.ReferenceMap[s.GetReflectType()]; ok {
		s.ResetName(name)
	}
	switch s := s.(type) {
	case shape.Container:
		switch len(s.Args) {
		case 1:
			return "[]" + v.getTypeName(s.Args[0])
		case 2:
			return strings.Repeat("*", s.GetLv()) + strings.TrimPrefix(s.GetPackage()+"."+s.GetName(), ".") + "[" + v.getTypeName(s.Args[0]) + "]" + v.getTypeName(s.Args[1])
		default:
			panic("hmm")
		}
	default:
		return strings.Repeat("*", s.GetLv()) + strings.TrimPrefix(s.GetPackage()+"."+s.GetName(), ".")
	}
}

type Config struct {
	BufferMap    map[string]io.ReadWriter
	ReferenceMap map[reflect.Type]string
	Writer       io.Writer
}

func (c *Config) Gen(ob interface{}) error {
	log.Printf("struct gen	%[1]T\n", ob)

	bufMap := c.BufferMap
	if bufMap == nil {
		bufMap = map[string]io.ReadWriter{}
	}
	refMap := c.ReferenceMap
	if refMap == nil {
		refMap = map[reflect.Type]string{}
	}
	w := c.Writer
	if w == nil {
		w = os.Stdout
	}

	v := &Visitor{
		BufferMap:    bufMap,
		ReferenceMap: refMap,
		OnlyOne:      true,
	}

	e := shape.Extractor{Seen: map[reflect.Type]shape.Shape{}}
	s := e.Extract(ob)
	for _, sub := range e.Seen {
		v.Visit(sub)
	}
	if err := v.Visit(s); err != nil {
		fmt.Errorf("visit %w", err)
	}

	for _, buf := range v.BufferMap {
		if _, err := io.Copy(w, buf); err != nil {
			return fmt.Errorf("copy %w", err)
		}
	}
	return nil
}
