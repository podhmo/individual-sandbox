package main

import (
	"context"
	"log"
	"runtime"

	"github.com/k0kubun/pp"
	"golang.org/x/sync/errgroup"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	g, ctx := errgroup.WithContext(context.Background())
	_ = ctx
	fn := func() {
		g.Go(func() error {
			err := runtime.StartTrace()
			pp.Println(err)
			return err
		})
	}
	fn()
	g.Go(func() error {
		runtime.StopTrace()
		return nil
	})
	return g.Wait()
}
