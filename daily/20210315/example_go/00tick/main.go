package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Config struct {
	Min             time.Duration
	Max             time.Duration
	RandomizeFactor float64

	Factor float64
}

func nextWait(c *Config, current time.Duration) time.Duration {
	r := 1.0
	if c.RandomizeFactor > 0 {
		f := c.RandomizeFactor
		if f > 1 {
			f = 1
		}
		r = (rand.Float64()-0.5)*2*f + 1
	}
	return time.Duration(int64(float64(current) * c.Factor * r))
}

func main() {
	c := &Config{
		Min:             5 * time.Second,
		Max:             160 * time.Second,
		Factor:          2,
		RandomizeFactor: 0.2,
	}

	current := c.Min
	for i := 0; i < 30; i++ {
		next := nextWait(c, current)
		fmt.Printf("%+30v -> %+30v  %v\n", current, next, current >= c.Max)
		current = next
	}
}
