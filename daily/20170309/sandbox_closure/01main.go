package main

import "fmt"

func g0(n int) []func() {
	fns := make([]func(), 0, n)
	for i := 0; i < n; i++ {
		fns = append(fns, func() { fmt.Printf("%d\n", i*i) })
	}
	return fns
}

func g1(n int) []func() {
	fns := make([]func(), 0, n)
	for i := 0; i < n; i++ {
		i := i
		fns = append(fns, func() { fmt.Printf("%d\n", i*i) })
	}
	return fns
}

func g2(n int) []func() {
	fns := make([]func(), 0, n)
	for i := 0; i < n; i++ {
		i := i
		fns = append(fns, func() { fmt.Printf("%d\n", i*i) })
	}
	return fns
}

func main() {
	{
		fns := g0(5)
		for _, fn := range fns {
			fn()
		}
	}
	{
		fns := g1(5)
		for _, fn := range fns {
			fn()
		}
	}
}
