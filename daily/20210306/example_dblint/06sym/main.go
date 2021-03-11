package main

import (
	"fmt"
	"runtime"
	"runtime/debug"
)

func main() {
	Foo()
}

func Foo() {
	Bar()
}

func Bar() {
	fmt.Println("----------------------------------------")
	debug.PrintStack()
	fmt.Println("----------------------------------------")
	pc, _, _, _ := runtime.Caller(0)
	fmt.Printf("%#+v\n", runtime.FuncForPC(pc).Name())
	frames := runtime.CallersFrames([]uintptr{pc})
	for {
		f, more := frames.Next()
		fmt.Println("@@", f.Function)
		fmt.Printf("%#+v\n", f)
		if !more {
			break
		}
	}
}
