package main

import "fmt"

func pair(x int) (int, int) {
	return x, x + 1
}

func add(x int, y int) int {
	return x + y
}

func main() {
	fmt.Println(add(pair(10)))
}
