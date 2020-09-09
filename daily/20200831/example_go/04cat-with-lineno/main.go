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

func (i *item) Do(do func()) {
	defer func() { i.ch <- struct{}{} }()
	do()
}

func files(ctx context.Context, names ...string) <-chan *item {
	ch := make(chan *item)
	go func() {
		defer close(ch)
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
		item.Do(func() {
			if item.err != nil {
				log.Println("!err", item.err)
				return
			}

			s := bufio.NewScanner(item.r)
			for s.Scan() {
				fmt.Println(i, s.Text())
			}
			if err := s.Err(); err != nil {
				cancel()
			}
		})
		i++
	}
	fmt.Println("ok")
	return nil
}
