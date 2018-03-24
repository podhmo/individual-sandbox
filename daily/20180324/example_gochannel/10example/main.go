package main

import (
	"log"
	"time"
)

func generator() chan int {
	ch := make(chan int)
	go func() {
		defer close(ch)
		ch <- 1
		time.Sleep(300 * time.Millisecond)
		ch <- 2
		time.Sleep(300 * time.Millisecond)
		ch <- 3
	}()
	return ch
}

func main() {
	for n := range generator() {
		log.Println(n)
	}
}
