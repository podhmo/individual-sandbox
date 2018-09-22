package main

import (
	"fmt"
	"runtime"
)

func main() {
	f()
}

func f() {
	g()
}

func g() {
	const depth = 32
	var pcs [depth]uintptr
	n := runtime.Callers(1, pcs[:])
	for _, pc := range pcs[0:n] {
		fn := runtime.FuncForPC(uintptr(pc))
		if fn == nil {
			fmt.Println("<unknown>")
			continue
		}
		file, lineno := fn.FileLine(uintptr(pc))
		fmt.Println(file, ":", lineno)
	}
}
