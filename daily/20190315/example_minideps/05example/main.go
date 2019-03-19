package main

import (
	"context"
	"fmt"
	"log"
	minideps "m/minideps2"
	"sync"
	"time"

	"golang.org/x/sync/errgroup"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel()

	g, _ := errgroup.WithContext(ctx)

	ch00 := make(chan int)
	ch10 := make(chan int)
	ch11 := make(chan int)
	ch21 := make(chan int)
	endChannel0 := make(chan []int, 1)
	endChannel1 := make(chan []int, 1)

	/*
	   {p0, p1} -> c0
	   {p1, p2} -> c1
	*/

	// TODO: state.Activatedを見るテスト
	minideps.Run(func(deps *minideps.Graph) {
		p0 := deps.NewNode("p0", func(s minideps.State) {
			g.Go(func() error {
				defer close(ch00)
				if s.Disabled {
					return nil
				}
				xs := []int{1, 2, 3, 4, 5}
				for _, x := range xs {
					ch00 <- x
				}
				return nil
			})
		})
		p1 := deps.NewNode("p1", func(s minideps.State) {
			g.Go(func() error {
				defer close(ch10)
				defer close(ch11)
				if s.Disabled {
					return nil
				}

				xs := []int{-1, -2, -3, -4, -5}
				for _, x := range xs {
					ch10 <- x
					ch11 <- x
				}
				return nil
			})
		})
		p2 := deps.NewNode("p2", func(s minideps.State) {
			g.Go(func() error {
				defer close(ch21)
				if s.Disabled {
					return nil
				}

				xs := []int{-1, -2, 1, 2}
				for _, x := range xs {
					ch21 <- x
				}
				return nil
			})
		})

		c0 := deps.NewNode("c0", func(s minideps.State) {
			g.Go(func() error {
				var r0 []int
				var r1 []int
				defer close(endChannel0)
				var wg sync.WaitGroup

				wg.Add(2)
				go func() {
					defer wg.Done()
					for x := range ch00 {
						r0 = append(r0, x)
					}
				}()
				go func() {
					defer wg.Done()
					for x := range ch10 {
						r1 = append(r1, x)
					}
				}()
				wg.Wait()
				if s.Disabled {
					return nil
				}
				endChannel0 <- append(r0, r1...)
				return nil
			})
		})

		c1 := deps.NewNode("c1", func(s minideps.State) {
			g.Go(func() error {
				var r0 []int
				var r1 []int
				defer close(endChannel1)
				var wg sync.WaitGroup

				wg.Add(2)
				go func() {
					defer wg.Done()
					for x := range ch11 {
						r0 = append(r0, x)
					}
				}()
				go func() {
					defer wg.Done()
					for x := range ch21 {
						r1 = append(r1, x)
					}
				}()
				wg.Wait()
				if s.Disabled {
					return nil
				}
				endChannel1 <- append(r1, r0...)
				return nil
			})
		})

		c0.Adjust(deps.WithDepends(p0, p1))
		c1.Adjust(deps.WithDepends(p1, p2), deps.WithDisabled())
	}, minideps.WithWrapFunction(func(s minideps.State, next func(minideps.State)) {
		fmt.Println("	run start", s.Name)
		defer fmt.Println("	run   end", s.Name)
		next(s)
	}))

	if err := g.Wait(); err != nil {
		panic(err)
	}
	fmt.Println(<-endChannel0)
	fmt.Println(<-endChannel1)

	// Output:
	// 1 2 3 4 5 -1 -2 -3 -4 -5
	// -1 -2 1 2 -1 -2 -3 -4 -5
	return nil
}
