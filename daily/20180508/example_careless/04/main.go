package main

import "fmt"

func main() {
	// map with pointer
	m := map[*int][]int{}
	xs := []int{1, 2, 3, 4, 5}

	for _, x := range xs {
		m[&x] = append(m[&x], x)
	}

	fmt.Printf("%+#v\n", m)
}
