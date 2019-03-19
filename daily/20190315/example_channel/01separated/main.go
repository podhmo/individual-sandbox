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

	provideA := NewProvider(func() {
		fmt.Println("start provider A")
		// providerA
		ch := make(chan int)
		g.Go(func() error {
			fmt.Println("start", "A")
			defer close(ch)
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

	provideB := NewProvider(func() {
		fmt.Println("start provider B")
		// providerB
		ch := make(chan string)
		g.Go(func() error {
			fmt.Println("start", "B")
			defer close(ch)
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

	provideC := NewProvider(func() {
		fmt.Println("start provider C")
		// providerC
		ch := make(chan string)
		g.Go(func() error {
			fmt.Println("start", "C")
			defer close(ch)
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

	consumeX := NewConsumer(g.Go, func() error {
		fmt.Println("start consumer X")
		var as []int
		var cs []string
		for x := range aForX {
			as = append(as, x)
		}
		for x := range cForX {
			cs = append(cs, x)
		}
		fmt.Println("X", as, cs)
		return nil
	})

	consumeY := NewConsumer(g.Go, func() error {
		fmt.Println("start consumer Y")
		fmt.Println("Y", <-aForY, <-bForY)
		return nil
	})

	consumeZ := NewConsumer(g.Go, func() error {
		fmt.Println("start consumer Z")
		var bs []string
		var cs []string
		for x := range bForZ {
			bs = append(bs, x)
		}
		for x := range cForZ {
			cs = append(cs, x)
		}
		fmt.Println("Z", bs, cs)
		return nil
	})

	// consumerX
	// consumeX = NewNoopConsumer()
	consumeX(provideA, provideC)

	// consumerY
	consumeY = NewNoopConsumer()
	consumeY(provideA, provideB)

	// consumerZ
	consumeZ = NewNoopConsumer()
	consumeZ(provideB, provideC)

	// drain
	g.Go(func() error {
		for x := range aForY {
			fmt.Println("	drain", x)
		}
		// for x := range bForY {
		// 	fmt.Println("	drain", x)
		// }
		// for x := range bForZ {
		// 	fmt.Println("	drain", x)
		// }
		for x := range cForZ {
			fmt.Println("	drain", x)
		}
		return nil
	})

	return g.Wait()
}

type provider struct {
	once sync.Once
	Fn   func()
}

func (p *provider) Provide() {
	p.once.Do(p.Fn)
}

// NewProvider :
func NewProvider(fn func()) func() {
	p := &provider{Fn: fn}
	return p.Provide
}

// consumer :
type consumer struct {
	Go func(func() error)
	Fn func() error
}

func (c *consumer) Consume(depends ...func()) {
	for i := range depends {
		depends[i]()
	}
	c.Go(c.Fn)
}

// NewConsumer :
func NewConsumer(Go func(func() error), fn func() error) func(depends ...func()) {
	c := &consumer{Go: Go, Fn: fn}
	return c.Consume
}

// noopConsumer :
type noopConsumer struct {
}

func (c *noopConsumer) Consume(depends ...func()) {
}

// NewNoopConsumer :
func NewNoopConsumer() func(depends ...func()) {
	c := &noopConsumer{}
	return c.Consume
}
