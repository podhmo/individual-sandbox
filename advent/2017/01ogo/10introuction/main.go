package main

import "fmt"

func main() {
	m := map[string]int{
		"f": 1,
		"g": 2,
		"h": 3,
	}
	for k, v := range m {
		fmt.Printf("%s %d ", k, v)
	}
	fmt.Println()
}
