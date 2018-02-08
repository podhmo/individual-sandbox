package main

import "fmt"

func add(n int) func(int, func(int)) {
	return func(v int, cont func(int)) {
		fmt.Println("input", v)
		r := v + n
		fmt.Printf("output (+ %d) %d\n", n, r)
		cont(r)
	}
}

func main() {
	i := 1
	add10 := add(10)
	add100 := add(100)

	add10(i, func(i int) {
		add100(i, func(i int) {
			fmt.Println("*fin*", i)
		})
	})
}
