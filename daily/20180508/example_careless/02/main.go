package main

import (
	"fmt"
)

func main() {
	// more defer
	fmt.Println("value", incByDefer())
	fmt.Println("pointer", *incByDefer2())
}

func incByDefer() int {
	i := 0
	defer func() {
		i++
	}()
	return i
}

func incByDefer2() *int {
	i := 0
	result := &i
	defer func() {
		i++
	}()
	return result
}
