package main

import "fmt"

func gen(n int) func() {
	for i := 0; i < n; i++ {
		if i == 5 {
			return func() {
				fmt.Printf("%d\n", i*i)
			}
		}
	}
	return func() {
		fmt.Println("hmm")
	}
}

func main() {
	gen(10)()
	gen(200)()
}
