package main

import (
	"fmt"
	"os"
	"os/signal"
	"time"
)

func main() {
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)

	ticker := time.NewTicker(time.Millisecond * time.Duration(100))
	tc := make(chan struct{}, 1)

loop:
	for {
		select {
		case s := <-c:
			switch s {
			case os.Interrupt:
				tc <- struct{}{}
			}
		case t := <-ticker.C:
			fmt.Println(t)
		case <-tc:
			fmt.Println("*stop*")
			ticker.Stop()
			break loop
		}
	}
}
