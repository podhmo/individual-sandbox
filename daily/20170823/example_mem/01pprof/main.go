package main

import (
	"fmt"
	"os"
	"runtime/pprof"
)

func f() {
	xs := make([]int, 10000)
	g()
	_ = xs
}

func g() {
	pprof.WriteHeapProfile(os.Stdout)
}

func main() {
	f()
	fmt.Println("----------------------------------------")
	pprof.WriteHeapProfile(os.Stdout)
}
