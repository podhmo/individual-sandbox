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
	r   io.Reader
	err error
	ch  chan<- struct{}
}

func files(ctx context.Context, names ...string) <-chan *item {
	ctx, cancel := context.WithCancel(ctx)
	ch := make(chan *item)
	go func() {
		defer close(ch)
		defer cancel()
		for i := range names {
			fname := names[i]
			log.Println("open", i, fname)
			f, err := os.Open(fname)
			sentinel := make(chan struct{})
			item := &item{r: f, ch: sentinel, err: err}
			ch <- item
			select {
			case <-ctx.Done():
				break
			case <-sentinel:
			}
			log.Println("close", i, fname)
			if f != nil {
				f.Close()
			}
		}
	}()
	return ch
}

func run() error {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	ch := files(ctx, os.Args[1:]...)
	i := 0
	for item := range ch {
		if item.err != nil {
			log.Println("!err", item.err)
			item.ch <- struct{}{}
			continue
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
	fmt.Println("ok")
	return nil
}
