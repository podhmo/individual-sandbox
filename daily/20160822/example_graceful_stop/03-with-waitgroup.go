package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

func calc(id int, n int) {
	st := time.Now()
	fmt.Printf("start(%2d): cost=%d\n", id, n)
	time.Sleep(time.Duration(n) * time.Millisecond)
	fmt.Printf(" end(%2d): cost=%d, time=%s\n", id, n, time.Now().Sub(st))
}

func main() {
	var wg sync.WaitGroup
	st := time.Now()

	for i := 0; i < 20; i++ {
		i := i
		wg.Add(1)
		go func() {
			calc(i, rand.Intn(2000))
			wg.Done()
		}()
	}
	wg.Wait()
	fmt.Printf("end with %s\n", time.Now().Sub(st))
}
