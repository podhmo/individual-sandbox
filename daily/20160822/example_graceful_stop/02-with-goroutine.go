package main

import (
	"fmt"
	"math/rand"
	"time"
)

func calc(id int, n int) {
	st := time.Now()
	fmt.Printf("start(%2d): cost=%d\n", id, n)
	time.Sleep(time.Duration(n) * time.Millisecond)
	fmt.Printf(" end(%2d): cost=%d, time=%s\n", id, n, time.Now().Sub(st))
}

func main() {
	st := time.Now()

	for i := 0; i < 20; i++ {
		go calc(i, rand.Intn(2000))
	}
	time.Sleep(time.Duration(5) * time.Second) // W: roughly waiting other go-routines.
	fmt.Printf("end with %s\n", time.Now().Sub(st))
}
