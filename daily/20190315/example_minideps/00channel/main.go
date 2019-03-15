package main

import (
	"context"
	"fmt"
	"log"
	"m/minideps"
	"strings"

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

	deps, start := minideps.New()
	produceA := deps.NewProducer(func(state minideps.State) {
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

	produceB := deps.NewProducer(func(state minideps.State) {
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

	produceC := deps.NewProducer(func(state minideps.State) {
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

	consumeX := deps.NewConsumer(func(state minideps.State) {
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

	consumeY := deps.NewConsumer(func(state minideps.State) {
		g.Go(func() error {
			fmt.Println("start consumer Y")
			fmt.Println("end consumer Y", state.Activated, <-aForY, <-bForY)
			return nil
		})
	})

	consumeZ := deps.NewConsumer(func(state minideps.State) {
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

	// consumer
	consumeY(deps.WithDeactivated(), produceA, produceB)
	consumeZ(deps.WithDeactivated(), produceB, produceC)
	consumeX(deps.WithActivated(), produceA, produceC)
	start()

	return g.Wait()
}
