package main

import (
	"fmt"
	"log"
	"time"
)

type future <-chan int

// Builder :
type Builder struct {
	Inspector *Inspector
}

// Inspector :
type Inspector struct {
	Before func(tag string, x interface{})
	After  func(tag string, x interface{})
}

func (b *Builder) newFuture(n int) future {
	promise := make(chan int)
	go func(n int) {
        log.Printf("%s before %v\n", tag, x)
		time.Sleep(1 * time.Second)
		promise <- n
        log.Printf("%s after %v\n", tag, x)
	}(n)
	return promise
}

func (b *Builder) addFuture(fut future, m int) future {
	promise := make(chan int)
	go func(m int) {
		n := <-fut
        log.Printf("%s before %v\n", tag, x)
		time.Sleep(1 * time.Second)
		promise <- m + n
        log.Printf("%s after %v\n", tag, x)
	}(m)
	return promise
}

func main() {
	b := &Builder{
		Inspector: &Inspector{
			Before: func(tag string, x interface{}) {

			},
			After: func(tag string, x interface{}) {
				log.Printf("%s after %v\n", tag, x)
			},
		},
	}

	fut := b.newFuture(1)
	fut2 := b.addFuture(fut, 2)
	fmt.Println("1 + 2 = ", <-fut2)
}
