package main

import "fmt"
import "golang.org/x/time/rate"

func main() {
	ch := make(chan int)
	go func() {
		ch <- 10
		ch <- 20
		ch <- 30
		close(ch)
	}()
	fmt.Println(<-ch)

	// これとおんなじ意味
	// for v := range ch {
	// 	fmt.Println(v)
	// }

loop:
	for {
		select {
		case v, ok := <-ch:
			if !ok {
				fmt.Println("end")
				break loop
			}
			fmt.Println(v)
		}
	}
}
