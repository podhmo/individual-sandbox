package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"log"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

type item struct {
	r  io.Reader
	ch chan<- struct{}
}

func files(ctx context.Context, errCh chan<- error, names ...string) <-chan *item {
	ctx, cancel := context.WithCancel(ctx)
	ch := make(chan *item)
	go func() {
		defer close(ch)
		defer cancel()
		for i := range names {
			fname := names[i]
			log.Println("open", i, fname)
			f, err := os.Open(fname)
			if err != nil {
				errCh <- err
				continue
			}

			sentinel := make(chan struct{})
			item := &item{r: f, ch: sentinel}
			ch <- item
			select {
			case <-ctx.Done():
				if err := ctx.Err(); err != nil {
					errCh <- err
				}
				log.Println("close", i, fname)
				f.Close()
				break
			case <-sentinel:
				log.Println("close", i, fname)
				f.Close()
			}
		}
	}()
	return ch
}

func run() error {
	errCh := make(chan error)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ch := files(ctx, errCh, os.Args[1:]...)
	i := 0
	for {
		select {
		case err := <-errCh:
			log.Println("!err", err)
		case item := <-ch:
			if item == nil {
				fmt.Println("ok")
				return nil
			}

			s := bufio.NewScanner(item.r)
			for s.Scan() {
				fmt.Println(i, s.Text())
			}
			if err := s.Err(); err != nil {
				cancel()
				return err
			}
			i++
			item.ch <- struct{}{}
		}
	}
}
