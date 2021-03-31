package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	time.Sleep(500 * time.Millisecond)

	fmt.Println(time.Since(now))
	fmt.Println(time.Since(now).Milliseconds())
}
