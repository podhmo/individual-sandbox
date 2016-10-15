package main

import (
	"log"
)

func doPrint(xs []interface{}) {
	for _, x := range xs {
		log.Printf("%[1]T: %#[1]v", x)
	}
}

func main() {
	{
		xs := []int{1, 2, 3, 4, 5}
		ys := make([]interface{}, len(xs))
		for i, x := range xs {
			ys[i] = x
		}
		doPrint(ys)
	}
	{
		xs := []string{"foo", "bar", "boo"}
		ys := make([]interface{}, len(xs))
		for i, x := range xs {
			ys[i] = x
		}
		doPrint(ys)
	}
}
