package main

import (
	"context"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/pkg/errors"
	"golang.org/x/sync/errgroup"
)

func main() {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	sigCh := make(chan os.Signal)
	go func() {
		select {
		case <-sigCh:
			cancel()
		}
	}()

	signal.Notify(
		sigCh,
		syscall.SIGINT,
		syscall.SIGTERM,
		syscall.SIGQUIT,
	)

	g, ctx := errgroup.WithContext(ctx)
	g.Go(func() error {
		for i := 0; i < 10; i++ {
			select {
			case <-ctx.Done():
				return ctx.Err()
			default:
			}
			log.Println(i)
			time.Sleep(500 * time.Millisecond)
		}
		return nil
	})

	if err := g.Wait(); err != nil {
		if errors.Cause(err) != context.Canceled {
			log.Fatal(err)
		}
		log.Println("teardown")
	}
}
