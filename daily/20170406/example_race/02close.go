package main

import (
	"fmt"
	"log"

	"github.com/wacul/ctxflow"
	"golang.org/x/net/context"
)

func run2() error {
	ch := make(chan int)
	go func() {
		for i := 1; i < 10; i++ {
			fmt.Println("tick", i)
			ch <- i
		}
	}()

	funcs := []ctxflow.FlowFunc{}
	xs := []int{0, 1, 2, 3, 4, 5}

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
		fmt.Println("close")
		close(ch)
	}()
	return ctxflow.ParallelMaxWorkersFunc(3, funcs...)(ctx)
}

func main() {
	if err := run2(); err != nil {
		log.Fatal(err)
	}
}
