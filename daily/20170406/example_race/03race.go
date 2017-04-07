package main

import (
	"fmt"
	"log"

	"sync"

	"github.com/wacul/ctxflow"
	"golang.org/x/net/context"
)

func run3() error {
	ch := make(chan int)
    N := 10
	var wg sync.WaitGroup
    wg.Add(N)

	go func() {
		for i := 1; i < N; i++ {
			fmt.Println("tick", i)
			ch <- i
            wg.Done()
		}
	}()

	funcs := []ctxflow.FlowFunc{}
	xs := []int{0, 1, 3, 3, 4, 5}

	for _, x := range xs {
		x := x
		funcs = append(funcs, func(ctx context.Context) (retErr error) {
			i := <-ch
			fmt.Println("x=", x, "i=", i, "x*i=", x*i)
			return nil
		})
	}
	ctx := context.Background()

	// panic
	go func() {
		fmt.Println("close?")
        wg.Wait()
		fmt.Println("close")
		close(ch)
	}()
	return ctxflow.ParallelMaxWorkersFunc(3, funcs...)(ctx)
}

func main() {
	if err := run3(); err != nil {
		log.Fatal(err)
	}
}
