package main

import "fmt"

func main() {
	ch := make(chan string, 5)
	for i := 0; i < 10; i++ {
		msg := fmt.Sprintf("add%d", i)
		select {
		case ch <- msg:
			fmt.Println(msg)
		default:
			fmt.Println("skip", msg)
		}
	}
	close(ch)
	for i := 0; i < 5; i++ {
		fmt.Println("return", <-ch)
	}
}
