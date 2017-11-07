package main

import (
	"context"
	"fmt"
	"log"
	"sync"
	"sync/atomic"

	"golang.org/x/sync/errgroup"
)

// StatefulGroup :
type StatefulGroup struct {
	g *errgroup.Group
	i *uint32

	mwg      sync.WaitGroup
	mError   error
	mErrOnce sync.Once
	mEndCh   chan struct{}
	mQueue   chan func() error
}

// WithContext :
func WithContext(ctx context.Context) (*StatefulGroup, context.Context) {
	g, ctx := errgroup.WithContext(ctx)
	i := uint32(0)
	end := make(chan struct{})
	queue := make(chan func() error, 8)
	return &StatefulGroup{g: g, mEndCh: end, i: &i, mQueue: queue}, ctx
}

// Go :
func (g *StatefulGroup) Go(fn func() error) {
	g.g.Go(fn)
}

// Modify :
func (g *StatefulGroup) Modify(fn func() error) {
	i := atomic.AddUint32(g.i, 1)
	if i == 1 {
		go func() {
			for {
				select {
				case <-g.mEndCh:
					return
				case fn := <-g.mQueue:
					err := fn()
					g.mwg.Done()
					if err != nil {
						g.mErrOnce.Do(func() {
							g.mError = err
						})
					}
				}
			}
		}()
	}
	g.mwg.Add(1)
	go func() {
		g.mQueue <- fn
	}()
}

// Wait :
func (g *StatefulGroup) Wait() error {
	err := g.g.Wait()
	g.mwg.Wait()
	g.mEndCh <- struct{}{}
	if err != nil {
		return err
	}
	return g.mError
}

// P :
type P struct {
	x int
	y int
	z int
}

func main() {
	ctx := context.Background()
	g, ctx := WithContext(ctx)
	p := P{}

	g.Go(func() error {
		if err := ctx.Err(); err != nil {
			return err
		}
		g.Modify(func() error {
			fmt.Println("x")
			p.x = 10
			return nil
		})
		return nil
	})
	g.Go(func() error {
		if err := ctx.Err(); err != nil {
			return err
		}
		g.Modify(func() error {
			fmt.Println("y")
			p.y = 100
			return nil
		})
		return nil
	})
	g.Go(func() error {
		if err := ctx.Err(); err != nil {
			return err
		}
		g.Modify(func() error {
			fmt.Println("z")
			p.z = 1000
			return nil
		})
		return nil
	})

	if err := g.Wait(); err != nil {
		log.Fatalf("hmm: %+v", err)
	}
	fmt.Println(p)
}
