package main

import (
	"fmt"
	"sync"
)

func main() {
	var n int
	var wg sync.WaitGroup
	var m sync.Mutex

	wg.Add(100)
	for i := 0; i < 100; i++ {
		go func() {
			defer wg.Done()
			m.Lock()
			defer m.Unlock()
			n++
		}()
	}
	wg.Wait()
	fmt.Println(n)
}
