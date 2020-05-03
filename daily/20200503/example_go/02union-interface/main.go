package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/k0kubun/pp"
)

type Kind string

const (
	KindAtom      Kind = "atom"
	KindComposite Kind = "composite"
)

type Node interface {
	Kind() Kind
}

type Atom struct {
	Type string `json:"type"`
}
type Composite struct {
	Type string `json:"type"`
	Args []Node `json:"args"`
}

func (x *Atom) Kind() Kind      { return KindAtom }
func (x *Composite) Kind() Kind { return KindComposite }

func NewAtom(typ string) Node {
	return &Atom{
		Type: typ,
	}
}

func NewComposite(typ string, args ...Node) Node {
	return &Composite{
		Type: typ,
		Args: args,
	}
}

func show(node Node) {
	switch node.(type) {
	case *Atom:
		fmt.Println("atom")
	case *Composite:
		fmt.Println("composite")
	}

	pp.Println(node)
}

func main() {
	show(NewAtom("string"))
	show(NewComposite("list", NewAtom("string")))

	fmt.Println("----------------------------------------")

	encoder := json.NewEncoder(os.Stdout)
	// encoder.SetIndent("", "  ")
	encoder.Encode(NewComposite("list", NewAtom("string")))

	// expect
	// {"$type": "composite", "type": "list", "args": [{"$type": "atom", "type": "string"}]}
	//
	// actual
	// {"type":"list","args":[{"type":"string"}]}
}
