package main

// time.Time to string by %s

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println(t)
	// 2016-08-18 19:54:52.105747747 +0900 JST
}
