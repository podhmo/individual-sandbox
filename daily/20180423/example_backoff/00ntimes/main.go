package main

import (
	"fmt"
	"time"

	"github.com/cenkalti/backoff"
)

func main() {
	for k := 0; k < 3; k++ {
		eb := backoff.NewExponentialBackOff()
		var total time.Duration

		for i := 0; i <= 5; i++ {
			d := eb.NextBackOff()
			fmt.Printf("%02d %s\n", i, d)
			total += d
		}
		fmt.Println("----------------------------------------")
		fmt.Println("total", total)
		fmt.Println("----------------------------------------")
	}
}
