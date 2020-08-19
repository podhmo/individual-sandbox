package main

import (
	"context"
	"fmt"
	"log"

	"github.com/cenkalti/backoff"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lmicroseconds)
	i := 0
	operation := func() error {
		log.Println("tick", i)
		if i > 10 {
			return nil
		}
		i++
		return fmt.Errorf("%d", i)
	}

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	b := backoff.NewExponentialBackOff()

	if err := backoff.Retry(
		operation,
		backoff.WithMaxRetries(backoff.WithContext(b, ctx), 5),
	); err != nil {
		log.Fatalf("!%+v", err)
	}
	return
}
