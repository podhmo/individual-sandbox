package main

import (
	"fmt"
	"math/rand"
	"sync"
)

func main() {
	var wg sync.WaitGroup
	m := map[int]int{}
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func(i int) {
			m[i] = rand.Int()
			wg.Done()
		}(i)
	}
	wg.Wait()
	fmt.Println(m)
}
