package main

import (
	"fmt"
	"io"
	"os"
)

type Named struct {
	Name string
	Decl *Struct
}

func (t Named) Emit(w io.Writer) {
	io.WriteString(w, t.Name)
}

type Struct struct {
	Fields []Field
}

type Field struct {
	Name     string
	Type     Type // xxx
	Required bool
}

type Type interface {
	Emit(io.Writer)
}
type Primitive struct {
	Name string
}

func (t Primitive) Emit(w io.Writer) {
	io.WriteString(w, t.Name)
}

type Nullable struct {
	Type Type
}

func (t Nullable) Emit(w io.Writer) {
	io.WriteString(w, "?")
	t.Type.Emit(w)
}

type Self struct {
}

func (t Self) Emit(w io.Writer) {
	io.WriteString(w, "<SELF>")
}

type OneOf []Type

func (t OneOf) Emit(w io.Writer) {
	io.WriteString(w, "(")
	t[0].Emit(w)
	for _, x := range t[1:] {
		io.WriteString(w, " | ")
		x.Emit(w)
	}
	io.WriteString(w, ")")
}

type Array struct {
	Type Type
}

func (t Array) Emit(w io.Writer) {
	io.WriteString(w, "[]")
	t.Type.Emit(w)
}

type Map struct {
	Key   Type
	Value Type
}

func (t Map) Emit(w io.Writer) {
	io.WriteString(w, "map[")
	t.Key.Emit(w)
	io.WriteString(w, "]")
	t.Value.Emit(w)
}

// type Person struct {
// 	Name string
// 	Age int
//  Nickname *string
// }

func Emit(w io.Writer, ob *Named) error {
	fmt.Fprintf(w, "type %s {\n", ob.Name)
	for _, f := range ob.Decl.Fields {
		io.WriteString(w, "\t")
		io.WriteString(w, f.Name)
		io.WriteString(w, " ")
		f.Type.Emit(w)
		if f.Required {
			io.WriteString(w, "!")
		}
		io.WriteString(w, "\n")
	}
	fmt.Fprintln(w, "}")
	return nil
}

func main() {
	// TODO: link
	// TODO: self recursion

	person := Named{
		Name: "Person",
		Decl: &Struct{
			Fields: []Field{
				{Name: "Name", Type: Primitive{"string"}, Required: true},
				{Name: "Age", Type: Primitive{"int"}},
				{Name: "Nickname", Type: Nullable{Primitive{"string"}}},
				{Name: "Father", Type: Nullable{Self{}}},
			},
		},
	}
	Emit(os.Stdout, &person)

	{
		ob := &Named{
			Name: "Team",
			Decl: &Struct{
				Fields: []Field{
					{Name: "Name", Type: Primitive{"string"}},
					{Name: "Member", Type: Array{OneOf{Self{}, person}}},
				},
			},
		}
		Emit(os.Stdout, ob)
	}

	w := os.Stdout

	{
		typ := Array{Array{Primitive{"string"}}}
		typ.Emit(w)
		fmt.Println("")
	}
	{
		typ := Map{Primitive{"string"}, Array{Primitive{"int"}}}
		typ.Emit(w)
		fmt.Println("")
	}
	{
		typ := Map{Primitive{"string"}, Array{OneOf{Primitive{"int"}, Primitive{"string"}}}}
		typ.Emit(w)
		fmt.Println("")
	}
}
