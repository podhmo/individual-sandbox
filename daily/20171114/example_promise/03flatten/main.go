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

func do(ctx context.Context, x interface{}) error {
	ctx, cancel := context.WithCancel(ctx)
	errch := make(chan error)

	ch0 := delayed(ctx, errch, x, 100*time.Millisecond)
	log.Println("..........")
	ch1 := make(chan interface{})
	defer cancel()
	select {
	case x := <-ch0:
		log.Printf("hoi: %v\n", x)
		cancel()
		go func() { ch1 <- x }()
	case err := <-errch:
		return err
	}

	ch2 := delayed(ctx, errch, x, 200*time.Millisecond)
	log.Println("..........")
	ch3 := make(chan interface{})

	select {
	case x := <-ch2:
		log.Printf("hoi: %v\n", x)
		go func() { ch3 <- x }()
	case err := <-errch:
		return err
	}
	return nil
}

func main() {
	x := 10
	ctx := context.Background()

	if err := do(ctx, x); err != nil {
		log.Println(err)
	}
	fmt.Println("byebye")
}
