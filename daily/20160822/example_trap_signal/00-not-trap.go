package main

import (
	"fmt"
	"time"
)


func main() {
	ticker := time.NewTicker(time.Millisecond * time.Duration(100))

	for {
		select {
		case t := <-ticker.C:
			fmt.Println(t)

		}
	}
}
