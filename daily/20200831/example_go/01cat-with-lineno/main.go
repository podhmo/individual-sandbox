package main

import (
	"bufio"
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

func files(errCh chan<- error, names ...string) <-chan *item {
	ch := make(chan *item)
	go func() {
		defer close(ch)
		for i := range names {
			fname := names[i]
			log.Println("open", i, fname)
			f, err := os.Open(fname)
			if err != nil {
				errCh <- err
				break
			}

			sentinel := make(chan struct{})
			item := &item{r: f, ch: sentinel}
			ch <- item
			<-sentinel
			log.Println("close", i, fname)
			f.Close()
		}
	}()
	return ch
}

func run() error {
	errCh := make(chan error)
	ch := files(errCh, os.Args[1:]...)
	i := 0
	for {
		select {
		case err := <-errCh:
			return err
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
				// TODO: stop fileCh
				return err
			}
			i++
			item.ch <- struct{}{}
		}
	}
}
