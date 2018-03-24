package main

import (
	"fmt"
	"sync"
)

func main() {
	var n int
	var wg sync.WaitGroup

	wg.Add(100)
	for i := 0; i < 100; i++ {
		go func() {
			defer wg.Done()
			n++
		}()
	}
	wg.Wait()
	fmt.Println(n)
}
