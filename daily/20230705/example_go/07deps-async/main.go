package main

import (
	"fmt"
	"log"
	"time"
)

func main() {
	log.SetFlags(log.Lmicroseconds)
	run()
}

func run() {
	now := time.Now()
	log.Printf("start %s", now)
	defer func() { log.Printf("end with %s", time.Since(now)) }()

	fmt.Println("----------------------------------------")
	fmt.Println("async")
	fmt.Println("----------------------------------------")
	fmt.Println(<-add(add(value(10), value(20)), add(value(30), value(40))))
}

func value(x int) <-chan int {
	ch := make(chan int)
	go func() {
		defer close(ch)
		time.Sleep(100 * time.Millisecond)
		log.Printf("value(%d)", x)
		ch <- x
	}()
	return ch
}
func add(xCh, yCh <-chan int) <-chan int {
	ch := make(chan int)
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
