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
func (c *cursor) Each2(ctx context.Context, fn func(int, *item) error) error {
	for {
		select {
		case <-ctx.Done():
			if err := ctx.Err(); err != nil {
				return err
			}
			return nil
		case item := <-c.ch:
			if item == nil {
				return nil
			}

			if err := fn(c.i, item); err != nil {
				c.cancel()
				item.ch <- struct{}{}
				return err
			}
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
	toplevel:
		for _, fname := range names {
			log.Println("open", cursor.i, fname)
			f, err := os.Open(fname)
			sentinel := make(chan struct{})
			item := &item{r: f, ch: sentinel, err: err}
			cursor.ch <- item
			select {
			case <-ctx.Done():
				if f != nil {
					log.Println("close!!", cursor.i, fname)
					f.Close()
				}
				break toplevel
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

	return files(ctx, os.Args[1:]...).Each2(ctx, func(i int, item *item) error {
		if item.err != nil {
			log.Println("!err", item.err)
			return item.err
		}

		s := bufio.NewScanner(item.r)
		for s.Scan() {
			fmt.Println(i, s.Text())
		}
		if err := s.Err(); err != nil {
			return err
		}
		return nil
	})
}
