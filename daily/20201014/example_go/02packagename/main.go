package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	nm "m/02packagename/go-name"
	"os"
	"strings"
	"time"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

type Visitor struct {
	Imports   []string
	BufferMap map[string]io.ReadWriter
	seen      []string
	OnlyOne   bool
}

// todo: relation ?

func (v *Visitor) Visit(s shape.Shape) error {
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

			if f.GetPackage() != "" {
				v.Imports = append(v.Imports, f.GetPackage())
			}

			fmt.Fprintf(w, "	%s %s%s", name, strings.Repeat("*", f.GetLv()), strings.TrimPrefix(f.GetReflectType().String(), f.GetPackage()+"."))
			if string(s.Tags[i]) != "" {
				fmt.Fprintf(w, " `%s`", s.Tags[i])
			}
			w.Write(nl)

			if !v.OnlyOne && f.GetPackage() == s.GetPackage() {
				if err := v.Visit(f); err != nil {
					return err
				}
			}
		}
		fmt.Fprintf(w, "}\n")
	case shape.Primitive, shape.Container:
		return nil
	default:
		return fmt.Errorf("unsupported type %T", s)
	}
	return nil // never
}

type Emitter struct {
	*Visitor
}

func (e *Emitter) Emit(w io.Writer) {
	fmt.Fprintln(w, "import (")
	for _, pkg := range e.Imports {
		if pkg == "main" {
			continue
		}
		fmt.Fprintf(w, "	%q\n", pkg)
	}
	fmt.Fprintln(w, ")")
	fmt.Fprintln(w, "")
	for _, k := range e.Visitor.seen {
		io.Copy(w, e.BufferMap[k])
	}
}

func main() {
	type Memo struct {
		Content string
	}
	type Person struct {
		Name nm.Name `json:"name"`
		Age  int     `json:"age"`

		CreatedAt time.Time `json:"createdAt"`
		Memo      Memo      `json:"memo"`
		Children  []Person
	}

	s := shape.Extract(Person{})
	v := &Visitor{
		BufferMap: map[string]io.ReadWriter{},
	}
	if err := v.Visit(s); err != nil {
		log.Fatalf("!!%v", err)
	}

	e := &Emitter{Visitor: v}
	e.Emit(os.Stdout)
}
