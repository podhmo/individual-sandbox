package main

import "fmt"

// closed channel returning zero value immediately
func main() {
	ch := make(chan struct{})
	select {
	case x := <-ch:
		fmt.Println("hai", x)
	default:
		fmt.Println("hmm")
	}
	close(ch)
	select {
	case x := <-ch:
		fmt.Println("hai", x)
	default:
		fmt.Println("hmm")
	}
}
