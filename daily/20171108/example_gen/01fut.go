package main

import (
	"fmt"
	"time"
)

func get() <-chan string {
	ch := make(chan string, 1)
	go func() {
		fmt.Println("get")
		time.Sleep(100 * time.Millisecond)
		fmt.Println("got")
		ch <- "hoi"
	}()
	return ch
}

func main() {
	fut := <-get()
	fmt.Println("hoi", fut)
}
