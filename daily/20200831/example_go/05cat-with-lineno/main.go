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

type cursor struct {
	i      int
	ch     chan *item
	cancel func()
}

func (c *cursor) Each(ctx context.Context, fn func(int, *item)) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	for {
		select {
		case <-ctx.Done():
			return
		case item := <-c.ch:
			if item == nil {
				return
			}
			fn(c.i, item)
			item.ch <- struct{}{}
			c.i++
		}
	}
}

type item struct {
	r   io.Reader
	err error
	ch  chan<- struct{}
}

func files(ctx context.Context, names ...string) *cursor {
	ctx, cancel := context.WithCancel(ctx)
	cursor := &cursor{ch: make(chan *item), cancel: cancel}
	defer cancel()

	go func() {
		defer close(cursor.ch)
		for _, fname := range names {
			log.Println("open", cursor.i, fname)
			f, err := os.Open(fname)
			sentinel := make(chan struct{})
			item := &item{r: f, ch: sentinel, err: err}
			cursor.ch <- item
			select {
			case <-ctx.Done():
				return
			case <-sentinel:
				log.Println("close!", cursor.i, fname)
				if f != nil {
					f.Close()
				}
			}
		}
	}()
	return cursor
}

func run() error {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	c := files(ctx, os.Args[1:]...)
	c.Each(ctx, func(i int, item *item) {
		if item.err != nil {
			log.Println("!err", item.err)
			return
		}

		s := bufio.NewScanner(item.r)
		for s.Scan() {
			fmt.Println(i, s.Text())
		}
		if err := s.Err(); err != nil {
			return
		}
	})
	return nil
}
