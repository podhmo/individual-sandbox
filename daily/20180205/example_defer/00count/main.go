package main

import "fmt"

func ng() {
	c := 0
	defer fmt.Println("ng", "c=", c)
	for i := 0; i < 10; i++ {
		c++
	}
}

func ok() {
	c := 0
	defer func(n *int) { fmt.Println("ok", "c=", *n) }(&c)
	for i := 0; i < 10; i++ {
		c++
	}
}

func main() {
	ng()
	ok()
}
