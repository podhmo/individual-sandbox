package main

import (
	"log"
	"sync"
)

func main() {
	N := 10
	vs := make([]int, N)
	for i := 0; i < N; i++ {
		vs[i] = i
	}

	for v := range run(vs) {
		log.Printf("\tanswer=%d", v)
	}
}

func run(vs []int) <-chan int {
	q := make(chan int)

	// producer
	go func() {
		for i := range vs {
			q <- i
		}
		close(q)
	}()

	r := make(chan int)

	var wg sync.WaitGroup

	// consumer
	for j := 0; j < 3; j++ {
		j := j
		wg.Add(1)
		go func() {
			for i := range q {
				v := vs[i]
				log.Printf("id=%d, v=%d", j, v)
				r <- v * v
			}
			wg.Done()
		}()
	}

	// closer
	go func() {
		wg.Wait()
		close(r)
	}()
	return r
}
