package main

import (
	"fmt"
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
	ch0 := generator()
	ch1 := generator()

	end0 := false
	end1 := true
loop:
	for {
		select {
		case n, ok := <-ch0:
			if !ok {
				end0 = true
				if end1 {
					break loop
				}
				continue
			}
			log.Println(0, n)
		case n, ok := <-ch1:
			if !ok {
				end1 = true
				if end0 {
					break loop
				}
				continue
			}
			log.Println(1, n)
		}
	}
	fmt.Println("end")
}
