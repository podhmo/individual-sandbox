package main

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

func main() {
	// type Person struct {
	// 	Name     *string `json:"name"`
	// 	Father   *Person
	// 	Mother   *Person
	// 	MetaData map[string]*Memo
	// }
	// type Memo struct {
	// 	Memo string `json:"memo"`
	// }

	memoT := reflect.StructOf([]reflect.StructField{
		{
			Name: "Memo",
			Type: reflect.TypeOf(""),
			Tag:  `json:"memo"`,
		},
	})

	personT := reflect.StructOf([]reflect.StructField{
		{
			Name: "Name",
			Type: reflect.PtrTo(reflect.TypeOf("")),
			Tag:  `json:"name"`,
		},
		{
			Name: "Father",
			Type: reflect.TypeOf(&Self{}),
		},
		{
			Name: "Mother",
			Type: reflect.TypeOf(&Self{}),
		},
		{
			Name: "MetaData",
			Type: reflect.MapOf(reflect.TypeOf(""), reflect.PtrTo(memoT)),
		},
	})

	v := &Visitor{
		BufferMap: map[string]io.ReadWriter{},
		ReferenceMap: map[reflect.Type]string{
			memoT:   "Memo",
			personT: "Person",
		},
	}

	s := shape.Extract(reflect.New(personT).Interface())
	if err := v.Visit(s); err != nil {
		log.Fatalf("!!%v", err)
	}

	for _, buf := range v.BufferMap {
		io.Copy(os.Stdout, buf)
	}
}

type Visitor struct {
	Imports   []string
	BufferMap map[string]io.ReadWriter
	seen      []string
	OnlyOne   bool

	ReferenceMap map[reflect.Type]string
}

func (v *Visitor) Visit(s shape.Shape) error {
	if name, ok := v.ReferenceMap[s.GetReflectType()]; ok {
		s.ResetName(name)
	}
	k := s.GetFullName()

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
		fmt.Fprintf(w, "type %s struct {\n", s.GetName())
		for i, name := range s.Fields.Keys {
			f := s.Fields.Values[i]

			if !v.OnlyOne && f.GetPackage() == s.GetPackage() {
				if err := v.Visit(f); err != nil {
					return err
				}
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
			if !v.OnlyOne && f.GetPackage() == s.GetPackage() {
				if err := v.Visit(f); err != nil {
					return err
				}
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
