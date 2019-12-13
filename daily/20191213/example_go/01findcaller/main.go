package main

import (
	"fmt"
	"runtime"
)

func findCaller(depth int) string {
	_, filename, lineno, _ := runtime.Caller(depth)
	return fmt.Sprintf("%d:%s", lineno, filename)
}

func main() {
	foo()
}

func foo() {
	bar()
}

func bar() {
	fmt.Println(findCaller(1))
}
