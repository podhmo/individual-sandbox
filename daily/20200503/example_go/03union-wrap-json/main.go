package main

import (
	"bytes"
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

func (node *Node) MarshalJSON() ([]byte, error) {
	switch node.Kind {
	case KindComposite:
		var inner struct {
			Kind Kind `json:"$kind"`
			*Composite
		}
		inner.Kind = node.Kind
		inner.Composite = node.Composite
		return json.Marshal(&inner)
	case KindAtom:
		var inner struct {
			Kind Kind `json:"$kind"`
			*Atom
		}
		inner.Kind = node.Kind
		inner.Atom = node.Atom
		return json.Marshal(&inner)
	default:
		return nil, fmt.Errorf("unexpected %q", node.Kind)
	}
}

func (node *Node) UnmarshalJSON(b []byte) error {
	var inner struct {
		Kind Kind `json:"$kind"`
	}
	if err := json.Unmarshal(b, &inner); err != nil {
		return err
	}

	node.Kind = inner.Kind
	switch node.Kind {
	case KindAtom:
		var inner Atom
		if err := json.Unmarshal(b, &inner); err != nil {
			return err
		}
		node.Atom = &inner
		return nil
	case KindComposite:
		var inner Composite
		if err := json.Unmarshal(b, &inner); err != nil {
			return err
		}
		node.Composite = &inner
		return nil
	default:
		return fmt.Errorf("unexpected %q", node.Kind)
	}
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
	// {"$kind": "composite", "type": "list", "args": [{"$kind": "atom", "type": "string"}]}
	//
	// actual
	// {"kind":"composite","composite":{"type":"list","args":[{"kind":"atom","atom":{"type":"string"}}]}}

	{
		s := `{"$kind":"composite","type":"list","args":[{"$kind":"atom","type":"string"}]}`
		var node Node
		decoder := json.NewDecoder(bytes.NewBufferString(s))
		decoder.Decode(&node)
		pp.Println(node)
	}
}
