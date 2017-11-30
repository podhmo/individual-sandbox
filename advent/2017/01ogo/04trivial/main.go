package main

import (
	"fmt"
)

func main() {
	m := map[string]int{
		"a": int('a') - '0',
		"b": int('b') - '0',
		"c": int('c') - '0',
		"d": int('d') - '0',
		"e": int('e') - '0',
		"f": int('f') - '0',
		"g": int('g') - '0',
		"h": int('h') - '0',
		"i": int('i') - '0', // broken order
	}
	fmt.Println("before")
	for k, v := range m {
		fmt.Println(k, v)
	}
	fmt.Println("after")
}
