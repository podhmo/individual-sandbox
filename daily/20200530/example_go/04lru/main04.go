package main

import (
	"fmt"

	lru "github.com/hashicorp/golang-lru"
)

func main() {
	c, _ := lru.New(8)
	for i := 0; i < 10; i++ {
		c.Add(i, nil)
	}

	fmt.Println(c.Get(0))
	fmt.Println(c.Get(9))
}
