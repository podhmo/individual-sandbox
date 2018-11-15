package main

import (
	"context"
	"log"
	"math/rand"
	"time"

	"github.com/vbauerster/mpb"
	"golang.org/x/sync/errgroup"
)

func init() {
	rand.Seed(time.Now().UnixNano())
}

func main() {
	g, _ := errgroup.WithContext(context.Background())
	p := mpb.New()
	total, numBars := 100, 3
	for i := 0; i < numBars; i++ {
		bar := p.AddBar(int64(total))

		// simulating some work
		g.Go(func() error {
			max := 100 * time.Millisecond
			for i := 0; i < total; i++ {
				start := time.Now()
				time.Sleep(time.Duration(rand.Intn(10)+1) * max / 10)
				// ewma based decorators require work duration measurement
				bar.IncrBy(1, time.Since(start))
			}
			return nil
		})
	}
	if err := g.Wait(); err != nil {
		log.Fatal(err)
	}
}
