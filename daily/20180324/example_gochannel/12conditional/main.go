package main

import (
	"fmt"
	"log"
	"math/rand"
	"sync"
	"time"

	"github.com/pkg/errors"
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
func (c *Conditional) Wait() <-chan error {
	c.waitOnce.Do(func() {
		c.wg.Wait()
		close(c.errCh)
	})
	c.wg.Wait()
	return c.errCh
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
	for i := 0; i < 130; i++ {
		i := i

		c.Go(func() error {
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

	for err := range c.Wait() {
		log.Println("error", err)
	}
	fmt.Println("done")
}
