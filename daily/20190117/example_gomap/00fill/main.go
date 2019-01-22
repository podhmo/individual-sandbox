package main

import "fmt"

// M :
type M map[string]int

func main() {
	m := M{"one": 1}
	fmt.Printf("%+v\n", m)
	fill(m)
	fmt.Printf("%+v\n", m)
}

func fill(m M) {
	m["zero"] = 0
}
