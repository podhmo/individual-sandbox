package main

import "fmt"

type S struct {
	x int
	y int
}

func main() {
	// APIのresponseでnilは避けたかった。yを渡し忘れていた
	s := &S{x: 10}
	fmt.Println(s) // yがrequiredであるということを表すには?(validation)
}
