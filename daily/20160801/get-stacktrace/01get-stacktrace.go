package main

import (
	"fmt"
	"os"
	"runtime"
)

func f(n int) int {
	if n < 1 {
        printStack()
		return 1
	} else {
		return n * f(n-1)
	}
}

func printStack() {
	var buf [4096]byte
	n := runtime.Stack(buf[:], false)
	os.Stdout.Write(buf[:n])
}

func main() {
	fmt.Printf("f(n=%v) = %v\n", 5, f(5))
}
