package main

import "fmt"

func main() {
	fmt.Println(1, 2, 3)
	fmt.Printf("%d,%d,%d\n", 1, 2, 3)
	fmt.Printf("%[1]d,%[2]d\n", 1, 2, 3)
}
