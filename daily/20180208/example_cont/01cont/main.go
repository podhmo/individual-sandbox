package main

import "fmt"

func peek(op func(int, func(int))) func(int, func(int)) {
	return func(v int, cont func(int)) {
		fmt.Println("input", v)
		op(v, func(v2 int) {
			fmt.Println("output", v2)
			cont(v2)
		})
	}
}

func add(n int) func(int, func(int)) {
	return func(v int, cont func(int)) {
		cont(v + n)
	}
}

func main() {
	i := 1
	add10 := peek(add(10))
	add100 := peek(add(100))

	add10(i, func(i int) {
		add100(i, func(i int) {
			fmt.Println("*fin*", i)
		})
	})
}
