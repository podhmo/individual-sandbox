package main

import (
	"context"
	"fmt"
	"log"
	"strings"
	"sync"

	"golang.org/x/sync/errgroup"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	defer fmt.Println("fin.")

	ctx := context.Background()
	g, ctx := errgroup.WithContext(ctx)

	aForX := make(chan int)
	aForY := make(chan int)
	bForY := make(chan string)
	bForZ := make(chan string)
	cForX := make(chan string)
	cForZ := make(chan string)

	produceA := NewProducer(func(state State) {
		fmt.Println("start producer A")
		// producerA
		ch := make(chan int)
		g.Go(func() error {
			defer close(ch)
			if !state.Activated {
				fmt.Println("disabled", "A")
				return nil
			}
			fmt.Println("start", "A")
			for i := 0; i < 5; i++ {
				ch <- i
			}
			fmt.Println("end", "A")
			return nil
		})
		// modelatorI
		g.Go(func() error {
			fmt.Println("start", "i")
			n := 0
			defer close(aForX)
			defer close(aForY)
			for x := range ch {
				aForX <- x
				n += x
			}
			aForY <- n
			fmt.Println("end", "i")
			return nil
		})
	})

	produceB := NewProducer(func(state State) {
		fmt.Println("start producer B")
		// producerB
		ch := make(chan string)
		g.Go(func() error {
			defer close(ch)
			if !state.Activated {
				fmt.Println("disabled", "B")
				return nil
			}

			fmt.Println("start", "B")

			xs := []string{"a", "b", "c", "d", "f"}
			for _, x := range xs {
				ch <- x
			}
			fmt.Println("end", "B")
			return nil
		})
		// modelatorJ
		g.Go(func() error {
			fmt.Println("start", "j")
			var merged []string
			defer close(bForY)
			defer close(bForZ)
			for x := range ch {
				bForZ <- x
				merged = append(merged, x)
			}
			bForY <- strings.Join(merged, ", ")
			fmt.Println("end", "j")
			return nil
		})
	})

	produceC := NewProducer(func(state State) {
		fmt.Println("start producer C")
		// producerC
		ch := make(chan string)
		g.Go(func() error {
			defer close(ch)
			if !state.Activated {
				fmt.Println("disabled", "C")
				return nil
			}

			fmt.Println("start", "C")
			xs := []string{"x", "y"}
			for _, x := range xs {
				ch <- x
			}
			fmt.Println("end", "C")
			return nil
		})
		// modelatorJ
		g.Go(func() error {
			fmt.Println("start", "k")
			defer close(cForX)
			defer close(cForZ)
			for x := range ch {
				cForX <- x
				cForZ <- x
			}
			fmt.Println("end", "k")
			return nil
		})
	})

	consumeX := NewConsumer(func(state State) {
		g.Go(func() error {
			fmt.Println("start consumer X")
			var as []int
			var cs []string
			for x := range aForX {
				as = append(as, x)
			}
			for x := range cForX {
				cs = append(cs, x)
			}
			fmt.Println("end consumer X", state.Activated, as, cs)
			return nil
		})
	})

	consumeY := NewConsumer(func(state State) {
		g.Go(func() error {
			fmt.Println("start consumer Y")
			fmt.Println("end consumer Y", state.Activated, <-aForY, <-bForY)
			return nil
		})
	})

	consumeZ := NewConsumer(func(state State) {
		g.Go(func() error {
			fmt.Println("start consumer Z")
			var bs []string
			var cs []string
			for x := range bForZ {
				bs = append(bs, x)
			}
			for x := range cForZ {
				cs = append(cs, x)
			}
			fmt.Println("end consumer Z", state.Activated, bs, cs)
			return nil
		})
	})

	// consumerX
	consumeX(WithActivated(), produceA, produceC)

	// consumerY
	consumeY(WithDeactivated(), produceA, produceB)

	// consumerZ
	consumeZ(WithDeactivated(), produceB, produceC)

	return g.Wait()
}

type producer struct {
	once sync.Once
	Fn   func(state State)
}

func (p *producer) Produce(state State) {
	p.once.Do(func() { p.Fn(state) })
}

// NewProducer :
func NewProducer(fn func(state State)) func(state State) {
	p := &producer{Fn: fn}
	return p.Produce
}

type State struct {
	Activated bool
}

// consumer :
type consumer struct {
	State *State
	Fn    func(state State)
}

func (c *consumer) Consume(opt func(state *State), depends ...func(state State)) {
	opt(c.State)
	for i := range depends {
		depends[i](*c.State)
	}
	c.Fn(*c.State)
}

// NewConsumer :
func NewConsumer(fn func(state State)) func(opt func(state *State), depends ...func(state State)) {
	c := &consumer{Fn: fn, State: &State{Activated: true}}
	return c.Consume
}

func WithDeactivated() func(state *State) {
	return func(state *State) {
		state.Activated = false
	}
}
func WithActivated() func(state *State) {
	return func(state *State) {
		state.Activated = true
	}
}
