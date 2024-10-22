package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"time"
)

type Node interface {
	Do() int
	Dump(w io.Writer)
}

type ValueNode struct {
	N int
}

func (n *ValueNode) Do() int {
	x := n.N
	time.Sleep(100 * time.Millisecond)
	log.Printf("value(%d)", x)
	return x
}
func (n *ValueNode) Dump(w io.Writer) {
	fmt.Fprintf(w, "value(%d)", n.N)
}

type AddNode struct {
	X Node
	Y Node
}

func (n *AddNode) Do() int {
	x := n.X.Do()
	y := n.Y.Do()
	time.Sleep(500 * time.Millisecond)
	log.Printf("add(%d,%d) = %d", x, y, x+y)
	return x + y
}
func (n *AddNode) Dump(w io.Writer) {
	fmt.Fprintf(w, "add(")
	n.X.Dump(w)
	fmt.Fprintf(w, ", ")
	n.Y.Dump(w)
	fmt.Fprintf(w, ")")
}

func main() {
	log.SetFlags(log.Lmicroseconds)
	now := time.Now()
	log.Printf("start %s", now)
	defer func() { log.Printf("end with %s", time.Since(now)) }()

	fmt.Println("----------------------------------------")
	fmt.Println("sync")
	fmt.Println("----------------------------------------")
	g := &AddNode{&AddNode{&ValueNode{10}, &ValueNode{20}}, &AddNode{&ValueNode{30}, &ValueNode{40}}}
	g.Dump(os.Stdout)
	fmt.Println("")
	fmt.Println("----------------------------------------")
	fmt.Println(g.Do())
}
