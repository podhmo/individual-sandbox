package main

import (
	"errors"
	"fmt"
	"log"
	"time"

	"github.com/cenkalti/backoff"
)

// WithTimeout :
func WithTimeout(b backoff.BackOff, timeout time.Duration) backoff.BackOff {
	return &backOffTimeout{delegate: b, timeout: timeout}
}

type backOffTimeout struct {
	delegate    backoff.BackOff
	timeout     time.Duration
	accumulated time.Duration
}

func (b *backOffTimeout) NextBackOff() time.Duration {
	d := b.delegate.NextBackOff()
	b.accumulated += d
	if b.accumulated > b.timeout {
		return backoff.Stop
	}
	return d
}

func (b *backOffTimeout) Reset() {
	var d time.Duration
	b.accumulated = d
	b.delegate.Reset()
}

func main() {
	{
		fmt.Println("----------------------------------------")
		fmt.Println("full")
		fmt.Println("----------------------------------------")

		var b backoff.BackOff
		b = backoff.NewConstantBackOff(200 * time.Millisecond)
		b = backoff.WithMaxRetries(b, 5)

		st := time.Now()
		fmt.Println("@@full", backoff.Retry(func() error {
			log.Println("do something", time.Since(st))
			return errors.New("wait")
		}, b))
	}

	{
		fmt.Println("----------------------------------------")
		fmt.Println("timeout 0.6s")
		fmt.Println("----------------------------------------")

		var b backoff.BackOff
		b = backoff.NewConstantBackOff(200 * time.Millisecond)
		b = backoff.WithMaxRetries(b, 5)
		b = WithTimeout(b, 600*time.Millisecond)

		st := time.Now()
		fmt.Println("timeout", backoff.Retry(func() error {
			log.Println("do something", time.Since(st))
			return errors.New("wait")
		}, b))
	}
}
