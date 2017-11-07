package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"golang.org/x/sync/errgroup"
)

// Pool :
type Pool struct {
	N  int
	ch chan struct{}
	g  *errgroup.Group
}

// WithContext :
func WithContext(ctx context.Context, concurrency int) (*Pool, context.Context) {
	g, ctx := errgroup.WithContext(ctx)
	ch := make(chan struct{}, concurrency)
	pool := &Pool{N: concurrency, ch: ch, g: g}
	return pool, ctx
}

// Wait :
func (p *Pool) Wait() error {
	return p.g.Wait()
}

// Go :
func (p *Pool) Go(f func() error) {
	p.ch <- struct{}{}
	p.g.Go(func() error {
		err := f()
		<-p.ch
		return err
	})
}

func main() {
	ctx := context.Background()
	pool, ctx := WithContext(ctx, 4)
	fmt.Println("tasks 10, concurrency 4")

	for i := 0; i < 10; i++ {
		i := i
		log.Printf("... %3d\n", i)
		pool.Go(func() error {
			if err := ctx.Err(); err != nil {
				return err
			}
			log.Printf("-> %3d\n", i)
			time.Sleep(1 * time.Second)
			log.Printf("<- %3d\n", i)
			return nil
		})
	}
	if err := pool.Wait(); err != nil {
		log.Fatal(err)
	}
}
