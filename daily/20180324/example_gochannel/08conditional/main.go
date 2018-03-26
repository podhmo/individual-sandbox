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

// New :
func New() *Conditional {
	return &Conditional{errCh: make(chan error)}
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
func (c *Conditional) Go(f func() error) {
	c.wg.Add(1)
	go func() {
		defer c.wg.Done()
		if err := f(); err != nil {
			c.errCh <- err
		}
	}()
}

func main() {
	c := New()
	sem := semaphore.NewWeighted(2)

	for i := 0; i < 5; i++ {
		i := i

		c.Go(func() error {
			sem.Acquire(context.Background(), 1)
			defer sem.Release(1)

			n := int(rand.Float64() * 1000)
			log.Println("start", i, n)
			time.Sleep(time.Duration(n) * time.Millisecond)
			if n%2 == 0 {
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
	fmt.Println("done")
}
