package limitedgroup

import (
	"context"

	"golang.org/x/sync/errgroup"
)

// LimitedGroup :
type LimitedGroup struct {
	g   *errgroup.Group
	N   int
	sem chan struct{}
}

// WithContext :
func WithContext(ctx context.Context, n int) (*LimitedGroup, context.Context) {
	g, ctx := errgroup.WithContext(ctx)
	sem := make(chan struct{}, n)
	go func() {
		select {
		case <-ctx.Done():
			close(sem)
			go func() {
				// drain
				for _ = range sem {
				}
			}()
		}
	}()
	return &LimitedGroup{g: g, N: n, sem: sem}, ctx
}

// Go :
func (g *LimitedGroup) Go(fn func() error) {
	g.sem <- struct{}{}
	g.g.Go(func() error {
		err := fn()
		<-g.sem
		return err
	})
}

// Wait :
func (g *LimitedGroup) Wait() error {
	return g.g.Wait()
}
