package main

import (
	"context"
	"fmt"
	"log"
	"math/rand"
	"sync"
	"time"

	"github.com/pkg/errors"
	"golang.org/x/sync/semaphore"
)

// Conditional :
type Conditional struct {
	wg       sync.WaitGroup
	waitOnce sync.Once
	errCh    chan error
}

// WithContext :
func WithContext(ctx context.Context) (*Conditional, context.Context) {
	return &Conditional{errCh: make(chan error)}, ctx
}

// WaitWith :
func (c *Conditional) WaitWith(fn func(errch <-chan error)) {
	go fn(c.errCh)
	c.waitOnce.Do(func() {
		c.wg.Wait()
		close(c.errCh)
	})
	c.wg.Wait()
}

// Go :
func (c *Conditional) Go(ctx context.Context, f func() error) {
	select {
	case <-ctx.Done():
		return
	default:
		c.wg.Add(1)
		go func() {
			defer c.wg.Done()
			select {
			case <-ctx.Done():
				return
			default:
				if err := f(); err != nil {
					c.errCh <- err
				}

			}
		}()
	}
}

func main() {
	c, ctx := WithContext(context.Background())
	ctx, cancel := context.WithCancel(ctx)
	sem := semaphore.NewWeighted(10)
	for i := 0; i < 130; i++ {
		i := i

		c.Go(ctx, func() error {
			sem.Acquire(context.Background(), 1)
			defer sem.Release(1)
			if i == 100 {
				fmt.Println("cancel")
				cancel()
			}
			n := int(rand.Float64() * 1000)
			log.Println("start", i, n)
			time.Sleep(time.Duration(n) * time.Millisecond)
			if n%8 == 0 {
				return errors.Errorf("%d %d", i, n)
			}
			log.Println("end  ", i, n)
			return nil
		})
	}

	c.WaitWith(func(errch <-chan error) {
		for err := range errch {
			log.Println("error", err)
		}
	})
	cancel()
	fmt.Println("done")
}
