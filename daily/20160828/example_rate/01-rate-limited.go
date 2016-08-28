package main

import (
	"fmt"
	"golang.org/x/net/context"
	"golang.org/x/time/rate"
	"log"
	"time"
)

func main() {
	const (
		N int = 100000
		M int = 10000
	)

	log.SetFlags(0)

	c := make(chan int, N)
	go func() {
		ctx := context.Background()
		n := rate.Every(time.Second / time.Duration(M))
		l := rate.NewLimiter(n, M)

		for i := 0; i < N; i++ {
			if err := l.Wait(ctx); err != nil {
				log.Fatalln(err)
			}
			c <- i
		}
		close(c)
	}()
	for n := range c {
		fmt.Println(n)
	}
}
