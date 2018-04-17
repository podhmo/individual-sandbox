package main

import (
	"log"
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

	sem := make(chan struct{}, 3)

	// consumer
	for j := 0; j < 3; j++ {
		j := j
		go func() {
			for i := range q {
				v := vs[i]
				log.Printf("id=%d, v=%d", j, v)
				r <- v * v
			}
			sem <- struct{}{}
		}()
	}

	// closer
	go func() {
		for i := 0; i < 3; i++ {
			<-sem
		}
		close(sem)
		close(r)
	}()
	return r
}
