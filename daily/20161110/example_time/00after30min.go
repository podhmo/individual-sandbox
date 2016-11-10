package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	fmt.Printf("%12s: %v\n", "now - 30min", now.Add(-30*time.Minute))
	fmt.Printf("%12s: %v\n", "now", now)
	fmt.Printf("%12s: %v\n", "now + 30min", now.Add(30*time.Minute))
}

/*
 now - 30min: 2016-11-10 10:47:24.533147102 +0900 JST
         now: 2016-11-10 11:17:24.533147102 +0900 JST
 now + 30min: 2016-11-10 11:47:24.533147102 +0900 JST
*/
