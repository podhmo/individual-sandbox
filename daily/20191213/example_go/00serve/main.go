package main

//from https://tennashi.hatenablog.com/entry/2019/12/13/010938

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"

	"golang.org/x/sync/errgroup"
)

func main() {
	os.Exit(run(context.Background()))
}

func run(ctx context.Context) int {
	var eg *errgroup.Group
	eg, ctx = errgroup.WithContext(ctx)

	eg.Go(func() error {
		return runServer(ctx)
	})
	eg.Go(func() error {
		return handleSignal(ctx)
	})
	eg.Go(func() error {
		<-ctx.Done()
		return ctx.Err()
	})

	if err := eg.Wait(); err != nil {
		fmt.Println(err)
		return 1
	}
	return 0
}

func handleSignal(ctx context.Context) error {
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt)

	select {
	case <-ctx.Done():
		signal.Reset()
		return nil
	case sig := <-sigCh:
		return fmt.Errorf("signal received: %v", sig.String())
	}
}

func runServer(ctx context.Context) error {
	s := &http.Server{
		Addr: ":8888",
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			fmt.Fprintln(w, "hello")
		}),
	}

	errCh := make(chan error)
	go func() {
		defer close(errCh)
		log.Println("listen", s.Addr)
		if err := s.ListenAndServe(); err != nil {
			errCh <- err
		}
	}()

	select {
	case <-ctx.Done():
		return s.Shutdown(ctx)
	case err := <-errCh:
		return err
	}
}
