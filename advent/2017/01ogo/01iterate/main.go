package main

import (
	"fmt"
	"strconv"
)

func main() {
	m := map[int]int{}
	for i := 0; i < 101; i++ {
		for _, c := range strconv.Itoa(i) {
			n := int(c) - '0'
			m[n]++
		}
	}

	fmt.Println("before")
	for k, v := range m {
		fmt.Println(k, v)
	}
	fmt.Println("after")
}
