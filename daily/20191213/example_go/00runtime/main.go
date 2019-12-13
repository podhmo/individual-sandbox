package main

import (
	"fmt"
	"runtime"
)

func main() {
	foo()
}

func foo() {
	bar()
}

func bar() {
	_, filename, lineno, _ := runtime.Caller(0)
	fmt.Printf("%d:%s\n", lineno, filename)
}
