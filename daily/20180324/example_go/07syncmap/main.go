package main

import (
	"fmt"
	"sync"
)

func main() {
	var wg sync.WaitGroup
	var c sync.Map

	wg.Add(100)
	for i := 0; i < 100; i++ {
		i := i
		go func() {
			c.Store(i, i*i)
			wg.Done()
		}()
	}
	wg.Wait()

	v, ok := c.Load(50)
	fmt.Println("50*50=", v.(int), ok)
}
