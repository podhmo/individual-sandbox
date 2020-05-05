package main

import (
	"github.com/podhmo/maperr"
	"encoding/json"
	"fmt"
	"strings"
)

type Tree struct {
	Kind TreeKind `json:"$kind"`
	Empty *Empty `json:"empty,omitempty"`
	Leaf *Leaf `json:"leaf,omitempty"`
	Node *Node `json:"node,omitempty"`
}

func (t *Tree) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Kind *TreeKind `json:"$kind"`// required
		Empty *json.RawMessage `json:"Empty"`
		Leaf *json.RawMessage `json:"Leaf"`
		Node *json.RawMessage `json:"Node"`
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Kind != nil  {
		t.Kind = *inner.Kind
	} else  {
		err = err.Add("$kind", maperr.Message{Text: "required"})
	}
	if inner.Empty != nil  {
		if rawerr := json.Unmarshal(*inner.Empty, &t.Empty); rawerr != nil  {
			err = err.Add("Empty", maperr.Message{Error: rawerr})
		}
	}
	if inner.Leaf != nil  {
		if rawerr := json.Unmarshal(*inner.Leaf, &t.Leaf); rawerr != nil  {
			err = err.Add("Leaf", maperr.Message{Error: rawerr})
		}
	}
	if inner.Node != nil  {
		if rawerr := json.Unmarshal(*inner.Node, &t.Node); rawerr != nil  {
			err = err.Add("Node", maperr.Message{Error: rawerr})
		}
	}

	// one-of?
	c := 0
	if t.Empty != nil  {
		c++
	}
	if t.Leaf != nil  {
		c++
	}
	if t.Node != nil  {
		c++
	}
	if c != 1  {
		err.Add("$kind", maperr.Message{Text: "not one-of"})
	}
	return err.Untyped()
}

type TreeKind string

const (
	TreeKindEmpty TreeKind = "Empty"
	TreeKindLeaf TreeKind = "Leaf"
	TreeKindNode TreeKind = "Node"
)


func (v TreeKind) Valid() error {
	switch v {
	case TreeKindEmpty, TreeKindLeaf, TreeKindNode:
		return nil
	default:
		return fmt.Errorf("%q is invalid enum value of (Empty, Leaf, Node)", v)
	}
}

func (v *TreeKind) UnmarshalJSON(b []byte) error {
	*v = TreeKind(strings.Trim(string(b), `"`))
	return v.Valid()
}


type Empty struct {

}

func (e *Empty) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {

	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check

	return err.Untyped()
}

type Leaf struct {
	Value int `json:"value"`
}

func (l *Leaf) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Value *int `json:"value"`// required
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Value != nil  {
		l.Value = *inner.Value
	} else  {
		err = err.Add("value", maperr.Message{Text: "required"})
	}

	return err.Untyped()
}

type Node struct {
	Left Tree `json:"left"`
	Right Tree `json:"right"`
}

func (n *Node) UnmarshalJSON(b []byte) error {
	var err *maperr.Error

	// loading internal data
	var inner struct {
		Left *json.RawMessage `json:"left"`// required
		Right *json.RawMessage `json:"right"`// required
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.AddSummary(rawErr.Error())
	}

	// binding field value and required check
	if inner.Left != nil  {
		n.Left = Tree{}
		if rawerr := json.Unmarshal(*inner.Left, &n.Left); rawerr != nil  {
			err = err.Add("left", maperr.Message{Error: rawerr})
		}
	} else  {
		err = err.Add("left", maperr.Message{Text: "required"})
	}
	if inner.Right != nil  {
		n.Right = Tree{}
		if rawerr := json.Unmarshal(*inner.Right, &n.Right); rawerr != nil  {
			err = err.Add("right", maperr.Message{Error: rawerr})
		}
	} else  {
		err = err.Add("right", maperr.Message{Text: "required"})
	}

	return err.Untyped()
}

func main() {

}
