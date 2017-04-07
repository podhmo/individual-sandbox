package main

import (
	"fmt"
	"log"

	"github.com/pkg/errors"
	"github.com/wacul/ctxflow"
	"golang.org/x/net/context"
)

func run(ctx context.Context) error {
	q := make(chan struct{}, 2)
	defer func() {
		fmt.Println("finish")
		close(q)
	}()

	funcs := []ctxflow.FlowFunc{}
	xs := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22}

	for _, x := range xs {
		x := x
		funcs = append(funcs, func(ctx context.Context) (retErr error) {
			defer func() {
				if err := recover(); err != nil {
					q <- struct{}{}
					retErr = errors.Errorf("err: %q", err)
				}
			}()
			fmt.Println(x / x)
			return nil
		})
	}
	return ctxflow.ParallelMaxWorkersFunc(10, funcs...)(ctx)
}

func main() {
	ctx := context.Background()
	err := run(ctx)
	if err != nil {
		log.Fatal(err)
	}
}
