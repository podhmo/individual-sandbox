package main

import (
	"context"
	"fmt"
	"log"
	"strings"

	"golang.org/x/sync/errgroup"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctx := context.Background()
	g, ctx := errgroup.WithContext(ctx)

	aForX := make(chan int)
	aForY := make(chan int)
	bForY := make(chan string)
	bForZ := make(chan string)
	cForX := make(chan string)
	cForZ := make(chan string)

	{
		// providerA
		ch := make(chan int)
		g.Go(func() error {
			defer close(ch)
			for i := 0; i < 5; i++ {
				ch <- i
			}
			fmt.Println("end", "A")
			return nil
		})
		// modelatorI
		g.Go(func() error {
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
	}
	{
		// providerB
		ch := make(chan string)
		g.Go(func() error {
			defer close(ch)
			xs := []string{"a", "b", "c", "d", "f"}
			for _, x := range xs {
				ch <- x
			}
			fmt.Println("end", "b")
			return nil
		})
		// modelatorJ
		g.Go(func() error {
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
	}
	{
		// providerC
		ch := make(chan string)
		g.Go(func() error {
			defer close(ch)
			xs := []string{"x", "y"}
			for _, x := range xs {
				ch <- x
			}
			fmt.Println("end", "c")
			return nil
		})
		// modelatorJ
		g.Go(func() error {
			defer close(cForX)
			defer close(cForZ)
			for x := range ch {
				cForX <- x
				cForZ <- x
			}
			fmt.Println("end", "k")
			return nil
		})
	}

	// consumerX
	g.Go(func() error {
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

	// consumerY
	g.Go(func() error {
		fmt.Println("Y", <-aForY, <-bForY)
		return nil
	})

	// consumerZ
	g.Go(func() error {
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
	return g.Wait()
}
