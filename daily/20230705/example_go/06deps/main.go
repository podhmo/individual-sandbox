package main

import (
	"fmt"
	"log"
	"time"
)

func main() {
	log.SetFlags(log.Lmicroseconds)
	runSync()
}

func runSync() {
	now := time.Now()
	log.Printf("start %s", now)
	defer func() { log.Printf("end with %s", time.Since(now)) }()

	fmt.Println("----------------------------------------")
	fmt.Println("sync")
	fmt.Println("----------------------------------------")
	fmt.Println(add(add(value(10), value(20)), add(value(30), value(40))))
}

func value(x int) int {
	time.Sleep(100 * time.Millisecond)
	log.Printf("value(%d)", x)
	return x
}

func add(x, y int) int {
	time.Sleep(500 * time.Millisecond)
	log.Printf("add(%d,%d) = %d", x, y, x+y)
	return x + y
}
