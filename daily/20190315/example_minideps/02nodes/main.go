package main

import (
	"context"
	"fmt"
	"log"
	minideps "m/minideps2"
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
	produceA := deps.NewNode("A", func(state minideps.State) {
		// producerA
		ch := make(chan int)
		g.Go(func() error {
			defer close(ch)
			if state.Disabled {
				fmt.Println("disabled", "A")
				return nil
			}
			fmt.Println("start", "A", state.Disabled)
			for i := 0; i < 5; i++ {
				ch <- i
			}
			fmt.Println("end", "A")
			return nil
		})
		// modelatorI
		g.Go(func() error {
			fmt.Println("start", "i", state.Disabled)
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

	produceB := deps.NewNode("B", func(state minideps.State) {
		// producerB
		ch := make(chan string)
		g.Go(func() error {
			defer close(ch)
			if state.Disabled {
				fmt.Println("disabled", "B")
				return nil
			}

			fmt.Println("start", "B", state.Disabled)

			xs := []string{"a", "b", "c", "d", "f"}
			for _, x := range xs {
				ch <- x
			}
			fmt.Println("end", "B")
			return nil
		})
		// modelatorJ
		g.Go(func() error {
			fmt.Println("start", "j", state.Disabled)
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

	produceC := deps.NewNode("C", func(state minideps.State) {
		// producerC
		ch := make(chan string)
		g.Go(func() error {
			defer close(ch)
			if state.Disabled {
				fmt.Println("disabled", "C")
				return nil
			}

			fmt.Println("start", "C", state.Disabled)
			xs := []string{"x", "y"}
			for _, x := range xs {
				ch <- x
			}
			fmt.Println("end", "C")
			return nil
		})
		// modelatorJ
		g.Go(func() error {
			fmt.Println("start", "k", state.Disabled)
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

	consumeX := deps.NewNode("X", func(state minideps.State) {
		g.Go(func() error {
			fmt.Println("start consumer X", state.Disabled)
			var as []int
			var cs []string
			for x := range aForX {
				as = append(as, x)
			}
			for x := range cForX {
				cs = append(cs, x)
			}
			fmt.Println("end consumer X", state.Disabled, as, cs)
			return nil
		})
	}, deps.WithDepends(produceA, produceC))

	consumeY := deps.NewNode("Y", func(state minideps.State) {
		g.Go(func() error {
			fmt.Println("start consumer Y", state.Disabled)
			fmt.Println("end consumer Y", state.Disabled, <-aForY, <-bForY)
			return nil
		})
	}, deps.WithDepends(produceA, produceB))

	consumeZ := deps.NewNode("Z", func(state minideps.State) {
		g.Go(func() error {
			fmt.Println("start consumer Z", state.Disabled)
			var bs []string
			var cs []string
			for x := range bForZ {
				bs = append(bs, x)
			}
			for x := range cForZ {
				cs = append(cs, x)
			}
			fmt.Println("end consumer Z", state.Disabled, bs, cs)
			return nil
		})
	}, deps.WithDepends(produceB, produceC))

	_ = consumeX
	consumeY.Adjust(deps.WithDisabled())
	consumeZ.Adjust(deps.WithDisabled())
	start()

	return g.Wait()
}
