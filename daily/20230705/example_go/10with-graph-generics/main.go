package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"time"
)

type Node[T any] interface {
	Do() <-chan T
	Dump(w io.Writer)
}

type ValueNode[T any] struct {
	X T
}

func (n *ValueNode[T]) Do() <-chan T {
	ch := make(chan T)
	go func() {
		defer close(ch)
		time.Sleep(100 * time.Millisecond)
		x := n.X
		log.Printf("value(%v)", x)
		ch <- x
	}()
	return ch
}
func (n *ValueNode[T]) Dump(w io.Writer) {
	fmt.Fprintf(w, "value(%v)", n.X)
}

type UopNode[T any] struct {
	X    Node[T]
	name string
	uOp  func(T) T
}

func (n *UopNode[T]) Do() <-chan T {
	xCh := n.X.Do()
	ch := make(chan T)
	go func() {
		defer close(ch)
		time.Sleep(100 * time.Millisecond)
		x := <-xCh
		log.Printf("%s(%v)", n.name, x)
		ch <- n.uOp(x)
	}()
	return ch
}
func (n *UopNode[T]) Dump(w io.Writer) {
	fmt.Fprintf(w, "%s(", n.name)
	n.X.Dump(w)
	fmt.Fprintf(w, ")")
}

type BopNode[T any] struct {
	X    Node[T]
	Y    Node[T]
	name string
	bOp  func(x, y T) T
}

func (n *BopNode[T]) Do() <-chan T {
	ch := make(chan T)
	xCh := n.X.Do()
	yCh := n.Y.Do()
	go func() {
		defer close(ch)
		time.Sleep(500 * time.Millisecond)
		x := <-xCh
		y := <-yCh
		ans := n.bOp(x, y)
		log.Printf("%s(%v,%v) = %v", n.name, x, y, ans)
		ch <- ans
	}()
	return ch
}
func (n *BopNode[T]) Dump(w io.Writer) {
	fmt.Fprintf(w, "%s(", n.name)
	n.X.Dump(w)
	fmt.Fprintf(w, ", ")
	n.Y.Dump(w)
	fmt.Fprintf(w, ")")
}

func value(n int) *ValueNode[int] {
	return &ValueNode[int]{X: n}
}
func add(x, y Node[int]) *BopNode[int] {
	return &BopNode[int]{X: x, Y: y, name: "add", bOp: func(x, y int) int { return x + y }}
}

func main() {
	log.SetFlags(log.Lmicroseconds)
	now := time.Now()
	log.Printf("start %s", now)
	defer func() { log.Printf("end with %s", time.Since(now)) }()

	fmt.Println("----------------------------------------")
	fmt.Println("async")
	fmt.Println("----------------------------------------")
	g := add(add(value(10), value(20)), add(value(30), value(40)))
	g.Dump(os.Stdout)
	fmt.Println("")
	fmt.Println("----------------------------------------")
	fmt.Println(<-g.Do())
}
