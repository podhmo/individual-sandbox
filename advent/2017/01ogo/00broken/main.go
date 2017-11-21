package main

import (
	"fmt"
	"sync"
)

func main() {
	m := map[int]int{}
	var wg sync.WaitGroup

	go func() {
		wg.Add(1)
		for j := 0; j < 10000; j++ {
			m[0]++
		}
	}()
	go func() {
		wg.Add(1)
		for j := 0; j < 10000; j++ {
			m[0]++
		}
	}()
	wg.Wait()
	fmt.Println(m)
}
