package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"github.com/cenkalti/backoff"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lmicroseconds)
	i := 0
	operation := func() error {
		if i > 10 {
			return nil
		}
		i++
		return fmt.Errorf("%d", i)
	}

	ticker := backoff.NewTicker(backoff.NewExponentialBackOff())

	var err error
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
toplevel:
	for {
		select {
		case c := <-ticker.C:
			if err = operation(); err != nil {
				log.Println(err, "will retry...", c)
			}
		case <-ctx.Done():
			if err = ctx.Err(); err != nil {
				log.Println(err, "hmm")
			}
			defer ticker.Stop()
			break toplevel
		}
	}
	return
}
