package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"time"
)

type Node interface {
	Do() <-chan int
	Dump(w io.Writer)
}

type ValueNode struct {
	N int
}

func (n *ValueNode) Do() <-chan int {
	x := n.N
	ch := make(chan int)
	go func() {
		defer close(ch)
		time.Sleep(100 * time.Millisecond)
		log.Printf("value(%d)", x)
		ch <- x
	}()
	return ch
}
func (n *ValueNode) Dump(w io.Writer) {
	fmt.Fprintf(w, "value(%d)", n.N)
}

type AddNode struct {
	X Node
	Y Node
}

func (n *AddNode) Do() <-chan int {
	ch := make(chan int)
	xCh := n.X.Do()
	yCh := n.Y.Do()
	go func() {
		defer close(ch)
		time.Sleep(500 * time.Millisecond)
		x := <-xCh
		y := <-yCh
		log.Printf("add(%d,%d) = %d", x, y, x+y)
		ch <- x + y
	}()
	return ch
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
	fmt.Println("async")
	fmt.Println("----------------------------------------")
	g := &AddNode{&AddNode{&ValueNode{10}, &ValueNode{20}}, &AddNode{&ValueNode{30}, &ValueNode{40}}}
	g.Dump(os.Stdout)
	fmt.Println("")
	fmt.Println("----------------------------------------")
	fmt.Println(<-g.Do())
}
