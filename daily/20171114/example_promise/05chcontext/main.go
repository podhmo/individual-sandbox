package main

import (
	"context"
	"log"
	"sync"
	"sync/atomic"
	"time"
)

// GiveupContext :
type GiveupContext interface {
	context.Context
	Giveup(err error) error
}

//errchContext :
type errchContext struct {
	ctx     context.Context
	err     error
	errOnce sync.Once
	errch   chan<- error
}

// WithChannel :
func WithChannel(ctx context.Context) (GiveupContext, <-chan error) {
	errch := make(chan error)
	return &errchContext{ctx: ctx, errch: errch}, errch
}

func (c *errchContext) storeErr(err error) {
	c.errOnce.Do(func() {
		c.errch <- err
		close(c.errch)
		c.err = err
	})
}

// Giveup :
func (c *errchContext) Giveup(err error) error {
	c.storeErr(err)
	return err
}

// Deadline :
func (c *errchContext) Deadline() (deadline time.Time, ok bool) {
	return c.ctx.Deadline()
}

// Done :
func (c *errchContext) Done() <-chan struct{} {
	return c.ctx.Done()
}

// Err :
func (c *errchContext) Err() error {
	return c.err
}

// Value :
func (c *errchContext) Value(key interface{}) interface{} {
	return c.ctx.Value(key)
}

var (
	i       *uint64
	errOnce sync.Once
)

func init() {
	x := uint64(0)
	i = &x
}

func delayed(ctx context.Context, x interface{}, n time.Duration) <-chan interface{} {
	ch := make(chan interface{})

	j := atomic.AddUint64(i, uint64(1))

	log.Printf("  start: %d (delayed %d)\n", j, n)
	go func() {
		select {
		case <-ctx.Done():
			log.Printf("  shutdown %d\n", j)
			close(ch)
			return
		case <-time.After(n):
			log.Printf("  end: %d\n", j)
			ch <- x
			return
		}
	}()
	return ch
}

func run(gctx GiveupContext, cancel func()) {
	ch := make(chan interface{})
	x := 10
	go func() {
		log.Printf("hoi: %v\n", x)
		tmpCH := delayed(gctx, x, 100*time.Millisecond)
		log.Println("..........")
		cancel()
		ch <- (<-tmpCH)
	}()

	go func() {
		y := <-ch // hmm nil is send
		log.Printf("hoi: %v\n", y)
		tmpCH := delayed(gctx, y, 100*time.Millisecond)
		log.Println("..........")
		<-tmpCH
		gctx.Giveup(nil)
	}()
}

func main() {
	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	gctx, errch := WithChannel(ctx)

	go func() { run(gctx, cancel) }()

	if err := <-errch; err != nil {
		log.Fatal(err)
	}
	log.Println("byebye")
}
