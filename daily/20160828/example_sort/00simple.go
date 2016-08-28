package main

import (
	"fmt"
	"sort"
)

type mystring string

func main() {
	xs := []string{"foo", "bar", "boo"}
	sort.Sort(sort.StringSlice(xs))

	fmt.Println(xs)
}
