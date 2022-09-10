package main

import (
	"fmt"
	"strings"
)

type Node interface {
	Value() int
	Add(int) Node
}

type Leaf struct {
	v int
}

func (l *Leaf) Value() int {
	return l.v
}

type Tree struct {
	v     int
	Left  Node
	Right Node
}

func (t *Tree) Value() int {
	return t.v
}

func (n *Leaf) Add(v int) Node {
	if n == nil {
		n = &Leaf{v: v}
		return n
	}
	t := &Tree{v: n.v}
	return t.Add(v)
}

func (n *Tree) Add(v int) Node {
	if n.v >= v {
		if n.Left == nil {
			n.Left = &Leaf{v}
		} else {
			n.Left = n.Left.Add(v)
		}
	} else {
		if n.Right == nil {
			n.Right = &Leaf{v}
		} else {
			n.Right = n.Right.Add(v)
		}
	}
	return n
}

func Walk(n Node, fn func(Node) bool) {
	switch n := n.(type) {
	case *Leaf:
		if !fn(n) {
			return
		}
	case *Tree:
		if !fn(n) {
			return
		}
		if n.Left != nil {
			Walk(n.Left, fn)
		}
		if n.Right != nil {
			Walk(n.Right, fn)
		}
	}
}

func Dump(n Node, lv int) {
	if n == nil {
		return
	}

	padding := strings.Repeat("@", lv*2)
	switch n := n.(type) {
	case *Leaf:
		fmt.Printf("%s%T[%v]\n", padding, n, n.Value())
	case *Tree:
		fmt.Printf("%s%T[%v]\n", padding, n, n.Value())
		if n.Left != nil {
			Dump(n.Left, lv+1)
		}
		if n.Right != nil {
			Dump(n.Right, lv+1)
		}
	}
}

func WalkWithStack(n Node, stack []Node, fn func([]Node) bool) {
	switch n := n.(type) {
	case *Leaf:
		stack = append(stack, n)
		if !fn(stack) {
			return
		}
	case *Tree:
		stack = append(stack, n)
		if !fn(stack) {
			return
		}
		if n.Left != nil {
			WalkWithStack(n.Left, stack, fn)
		}
		if n.Right != nil {
			WalkWithStack(n.Right, stack, fn)
		}
	}
}

func noop(Node) {}

func WalkWithCont(n Node, fn func(n Node, cont func(Node))) {
	switch n := n.(type) {
	case *Leaf:
		fn(n, noop)
	case *Tree:
		fn(n, func(Node) {
			if n.Left != nil {
				WalkWithCont(n.Left, fn)
			}
			if n.Right != nil {
				WalkWithCont(n.Right, fn)
			}
		})
	}
}

// ast.Inspect style
func Walk2(n Node, fn func(n Node) bool) {
	switch n := n.(type) {
	case *Leaf:
		if !fn(n) {
			return
		}
	case *Tree:
		if !fn(n) {
			return
		}
		if n.Left != nil {
			Walk2(n.Left, fn)
			fn(nil)
		}
		if n.Right != nil {
			Walk2(n.Right, fn)
			fn(nil)
		}
	}
}

func main() {
	var t Node = (*Leaf)(nil)
	t = t.Add(2).
		Add(7).
		Add(5).
		Add(2).
		Add(6).
		Add(5).
		Add(11).
		Add(5).
		Add(9).
		Add(4)

	// これだと構造を保てない
	Walk(t, func(n Node) bool {
		fmt.Println(n.Value())
		return true
	})

	fmt.Println("----------------------------------------")
	// これだと高階関数ではない
	Dump(t, 0)

	fmt.Println("----------------------------------------")
	// stackを取れればそれで解決するけれど。 (golang.org/x/tools/go/ast/inspector みたいな感じで)
	WalkWithStack(t, nil, func(s []Node) bool {
		n := s[len(s)-1]
		fmt.Printf("%s%T[%v]\n", strings.Repeat("@@", len(s)-1), n, n.Value())
		return true
	})

	fmt.Println("----------------------------------------")
	{
		var s []Node
		WalkWithCont(t, func(n Node, cont func(Node)) {
			s = append(s, n)
			fmt.Printf("%s%T[%v]\n", strings.Repeat("@@", len(s)-1), n, n.Value())
			cont(n)
			s = s[:len(s)-1] // pop
		})
	}

	fmt.Println("----------------------------------------")
	{
		var s []Node
		Walk2(t, func(n Node) bool {
			if n != nil {
				s = append(s, n)
				fmt.Printf("%s%T[%v]\n", strings.Repeat("@@", len(s)-1), n, n.Value())
			} else {
				s = s[:len(s)-1] // pop
			}
			return true
		})
	}
}
