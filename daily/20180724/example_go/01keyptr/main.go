package main

import "fmt"

func key(s string) *string {
	return &s
}

func main() {
	c := map[*string]int{}
	s := "foo"
	xs := []string{s, "bar", s}
	for _, x := range xs {
		c[key(x)] += 1
	}
	fmt.Printf("%+v", c)
}
