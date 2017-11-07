package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"golang.org/x/sync/errgroup"
)

// LimitedGroup :
type LimitedGroup struct {
	g  *errgroup.Group
	n  int
	ch chan struct{}
}

// WithContext :
func WithContext(ctx context.Context, n int) (*LimitedGroup, context.Context) {
	g, ctx := errgroup.WithContext(ctx)
	ch := make(chan struct{}, n)
	return &LimitedGroup{g: g, n: n, ch: ch}, ctx
}

// Go :
func (g *LimitedGroup) Go(fn func() error) {
	g.ch <- struct{}{}
	g.g.Go(func() error {
		err := fn()
		<-g.ch
		return err
	})
}

// Wait :
func (g *LimitedGroup) Wait() error {
	defer close(g.ch)
	return g.g.Wait()
}

func main() {
	ctx := context.Background()
	g, ctx := WithContext(ctx, 3)
	for i := 0; i < 10; i++ {
		i := i
		g.Go(func() error {
			if err := ctx.Err(); err != nil {
				return err
			}
			log.Printf("-> %d\n", i)
			time.Sleep(time.Duration(int(0.5 * float64(time.Second))))
			log.Printf("<- %d\n", i)
			return nil
		})
	}
	if err := g.Wait(); err != nil {
		log.Fatalf("hmm: %+v", err)
	}
	fmt.Println("ok")
}
