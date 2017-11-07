package main

import (
	"context"
	"log"
	"strings"
	"time"

	"golang.org/x/sync/errgroup"
)

func run(i int) error {
	log.Printf("-> %s%3d\n", strings.Repeat(" ", i), i)
	time.Sleep(1 * time.Second)
	log.Printf("<- %s%3d\n", strings.Repeat(" ", i), i)
	return nil
}

func main() {
	ctx := context.Background()
	g, ctx := errgroup.WithContext(ctx)
	ch := make(chan struct{}, 4)

	for i := 0; i < 10; i++ {
		i := i
		g.Go(func() error {
			log.Printf("enqueue %d\n", i)
			ch <- struct{}{}
			if err := ctx.Err(); err != nil {
				<-ch
				return err
			}
			err := run(i)
			<-ch
			return err
		})
	}

	if err := g.Wait(); err != nil {
		log.Fatal(err)
	}
}
