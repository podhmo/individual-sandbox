package main

import (
	"os"
	"runtime"
	"runtime/pprof"
)

func f() {
	xs := make([]int, 10000)
	g()
	_ = xs
}

func g() {
	p := pprof.Lookup("heap")
	runtime.GC()
	p.WriteTo(os.Stdout, 1)
}

func main() {
	oldRate := runtime.MemProfileRate
	runtime.MemProfileRate = 1
	defer func() {
		runtime.MemProfileRate = oldRate
	}()
	f()
}
