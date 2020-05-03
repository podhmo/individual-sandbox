package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/k0kubun/pp"
)

type Atom struct {
	Type string `json:"type"`
}

type Composite struct {
	Type string  `json:"type"`
	Args []*Node `json:"args"`
}

type Kind string

const (
	KindAtom      Kind = "atom"
	KindComposite      = "composite"
)

type Node struct {
	Kind      Kind       `json:"kind"`
	Atom      *Atom      `json:"atom,omitempty"`
	Composite *Composite `json:"composite,omitempty"`
}

func NewAtom(typ string) *Node {
	return &Node{
		Kind: KindAtom,
		Atom: &Atom{
			Type: typ,
		},
	}
}

func NewComposite(typ string, args ...*Node) *Node {
	return &Node{
		Kind: KindComposite,
		Composite: &Composite{
			Type: typ,
			Args: args,
		},
	}
}

func show(node *Node) {
	switch node.Kind {
	case KindAtom:
		fmt.Println("atom")
	case KindComposite:
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
	// {"kind":"composite","composite":{"type":"list","args":[{"kind":"atom","atom":{"type":"string"}}]}}
}
