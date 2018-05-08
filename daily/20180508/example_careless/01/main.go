package main

import "fmt"

func main() {
	// defer with counting
	c := 0
	defer func() {
		fmt.Println("with func", c)
	}()

	defer fmt.Println("without func", c)
	for i := 0; i < 3; i++ {
		defer fmt.Println(i)
		c++
	}
}
