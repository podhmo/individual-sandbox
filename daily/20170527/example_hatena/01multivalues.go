package main

import "fmt"

// NPlus :
func NPlus(n int) (int, int) {
	return n, n + 1
}

// Add :
func Add(x, y int) int {
	return x + y
}

func main() {
	fmt.Println(Add(NPlus(10)))
}
