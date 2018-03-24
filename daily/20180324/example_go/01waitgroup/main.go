package main

import (
	"fmt"
	"sync"
	"time"
)

func main() {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		time.Sleep(100 * time.Millisecond)
		fmt.Println("	one")
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		time.Sleep(100 * time.Millisecond)
		fmt.Println("	two")
		wg.Done()
	}()
	fmt.Println("wait...")
	wg.Wait()
	fmt.Println("done")
}
