package main

import "fmt"

func main() {
	// よく最初にあがる例(closure)

	var fns []func()
	for i := 0; i < 3; i++ {
		fns = append(fns, func() {
			fmt.Println(i)
		})
	}

	for _, fn := range fns {
		fn()
	}
}
