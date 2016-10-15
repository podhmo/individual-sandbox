package main

import (
	"log"
)

func doPrint(xs []interface{}) {
	for _, x := range xs {
		log.Printf("%T[1]: %#[1]v", x)
	}
}

func main() {
	xs := []int{1, 2, 3, 4, 5}
	// compile error. expected []interface{} but []int
	doPrint(xs)
}
