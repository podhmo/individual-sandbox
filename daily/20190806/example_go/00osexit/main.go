package main

import (
	"fmt"
	"os"
)

func main() {
	run()
	run2()
}

func run() {
	fmt.Println(1)
	defer fmt.Println(2)
	fmt.Println("@")
}

func run2() {
	fmt.Println(1)
	defer fmt.Println(2)
	fmt.Println("@")
	os.Exit(0)
}
