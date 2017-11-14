package main

import (
	"context"
	"fmt"
	"log"
	"sync/atomic"
	"time"
)

var (
	i *uint64
)

func init() {
	x := uint64(0)
	i = &x
}

func delayed(ctx context.Context, x interface{}, n time.Duration) <-chan interface{} {
	ch := make(chan interface{})

	j := atomic.AddUint64(i, uint64(1))

	log.Printf("  start: %d (delayed %d)\n", j, n)
	go func() {
		select {
		case <-ctx.Done():
			log.Printf("  shutdown %d\n", j)
			return
		case <-time.After(n):
			log.Printf("  end: %d\n", j)
			ch <- x
			return
		}
	}()
	return ch
}

func main() {
	x0 := 10
	ctx := context.Background()

	ch0 := delayed(ctx, x0, 100*time.Millisecond)

	log.Println("..........")

	x1 := <-ch0
	log.Printf("hoi: %v\n", x1)

	ch1 := delayed(ctx, x1, 200*time.Millisecond)

	log.Println("..........")

	x2 := <-ch1
	log.Printf("hoi: %v\n", x2)

	fmt.Println("byebye")
}
