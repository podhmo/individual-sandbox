package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"sync"
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
		// log.Printf("value(%v)", x)
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

func Run0[T any](name string, fn func() T) *UopNode[T] {
	return &UopNode[T]{
		name: name,
		X:    &ValueNode[T]{},
		uOp: func(x T) T {
			return fn()
		},
	}
}
func Run1[T any](name string, fn func(v T) T) *UopNode[T] {
	return &UopNode[T]{
		name: name,
		X:    &ValueNode[T]{},
		uOp:  fn,
	}
}
func Run2[T any](name string, fn func(x, y T) T) *BopNode[T] {
	return &BopNode[T]{
		name: name,
		X:    &ValueNode[T]{},
		bOp:  fn,
	}
}

func Serial(nodes ...Node[error]) error {
	for i, node := range nodes {
		if err := <-node.Do(); err != nil {
			buf := new(bytes.Buffer)
			node.Dump(buf)
			return fmt.Errorf("task(%d): %s -- %w", i, buf.String(), err)
		}
	}
	return nil
}
func Concurrent(nodes ...Node[error]) error {
	errCh := make(chan error, len(nodes))
	var wg sync.WaitGroup

	wg.Add(len(nodes))
	for i, node := range nodes {
		go func(i int, node Node[error]) {
			defer wg.Done()
			if err := <-node.Do(); err != nil {
				buf := new(bytes.Buffer)
				node.Dump(buf)
				errCh <- fmt.Errorf("task(%d): %s -- %w", i, buf.String(), err)
			}
		}(i, node)
	}
	wg.Wait()

	select {
	case err := <-errCh:
		return err
	default:
		return nil
	}
}

func main() {
	log.SetFlags(log.Lmicroseconds)
	now := time.Now()
	log.Printf("start %s", now)
	defer func() { log.Printf("end with %s", time.Since(now)) }()

	if err := Concurrent(
		Run0("foo", func() error {
			fmt.Println("start foo")
			time.Sleep(200 * time.Millisecond)
			fmt.Println("end foo")
			return nil
		}),
		Run0("bar", func() error {
			fmt.Println("start bar")
			time.Sleep(200 * time.Millisecond)
			fmt.Println("end bar")
			return nil
		}),
	); err != nil {
		panic(err)
	}
}
