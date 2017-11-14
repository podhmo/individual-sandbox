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

func do(ctx context.Context, x interface{}, errch chan<- error) {
	ctx, cancel := context.WithCancel(ctx)
	ch := make(chan interface{})
	go func() {
		log.Printf("hoi: %v\n", x)
		tmpCH := delayed(ctx, errch, x, 100*time.Millisecond)
		log.Println("..........")
		x := <-tmpCH
		cancel()
		ch <- x
	}()

	go func() {
		x := <-ch
		log.Printf("hoi: %v\n", x)
		tmpCH := delayed(ctx, errch, x, 100*time.Millisecond)
		log.Println("..........")
		x = <-tmpCH
		cancel()
		errch <- nil
	}()
}

func main() {
	x := 10
	ctx := context.Background()

	errch := make(chan error)
	go func() { do(ctx, x, errch) }()
	if err := <-errch; err != nil {
		log.Println(err)
	}
	fmt.Println("byebye")
}
