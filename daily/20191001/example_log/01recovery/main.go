package main

import (
	"context"
	"fmt"
	"log"

	"github.com/pkg/errors"
	"golang.org/x/sync/errgroup"
)

func main() {
	log.SetFlags(log.Lmicroseconds | log.Lshortfile)
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	g, _ := errgroup.WithContext(context.Background())
	type q struct {
		x int
	}
	type p struct {
		q *q
	}
	g.Go(func() (err error) {
		defer func() {
			if pErr := recover(); pErr != nil {
				switch pErr := pErr.(type) {
				case error:
					err = errors.Wrap(pErr, "panic")
				default:
					err = errors.Errorf("**%+v**", pErr)
				}
			}
		}()
		fmt.Println(new(p).q.x + new(p).q.x)
		return nil
	})
	return g.Wait()
}
