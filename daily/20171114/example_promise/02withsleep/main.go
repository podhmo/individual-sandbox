package main

import (
	"context"
	"fmt"
	"log"
	"sync"
	"sync/atomic"
	"time"
)

var (
	i       *uint64
	errOnce sync.Once
)

func init() {
	x := uint64(0)
	i = &x
}

func delayed(ctx context.Context, errch chan<- error, x interface{}, n time.Duration) <-chan interface{} {
	ch := make(chan interface{})

	j := atomic.AddUint64(i, uint64(1))

	log.Printf("  start: %d (delayed %d)\n", j, n)
	go func() {
		select {
		case <-ctx.Done():
			log.Printf("  shutdown %d\n", j)
			errOnce.Do(func() {
				errch <- ctx.Err()
				close(errch)
			})
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
	ctx, cancel := context.WithCancel(ctx)

	errch := make(chan error)
	ch0 := delayed(ctx, errch, x0, 100*time.Millisecond)

	log.Println("..........")

	select {
	case x1 := <-ch0:
		log.Printf("hoi: %v\n", x1)
		cancel()

		ch1 := delayed(ctx, errch, x1, 200*time.Millisecond)
		log.Println("..........")

		select {
		case x2 := <-ch1:
			log.Printf("hoi: %v\n", x2)
		case err := <-errch:
			log.Println(err)
		}
	case err := <-errch:
		log.Println(err)
		log.Println("canceled")
	}
	cancel()
	fmt.Println("byebye")
}
