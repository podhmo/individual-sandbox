package main

import (
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	xs := []int{2, 4, 6, 8}
	for i := 0; i < len(xs); i++ {
		fmt.Printf("%d, ", xs[i])
	}
	fmt.Println("")

	for i := len(xs) - 1; i > -1; i-- {
		fmt.Printf("%d, ", xs[i])
	}
	fmt.Println("")

	for i := len(xs) - 1; i > 0; i-- {
		fmt.Printf("(%d, %d), ", xs[i-1], xs[i])
	}

	fmt.Println("")
	for i := 1; i < len(xs); i++ {
		fmt.Printf("(%d, %d), ", xs[i-1], xs[i])
	}
	fmt.Println("")
	return nil
}
