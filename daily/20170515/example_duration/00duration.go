package main

import (
	"fmt"
	"time"
)

func main() {
	st := time.Now()
	time.Sleep(1500 * time.Millisecond)
	fmt.Println(time.Now().Sub(st).Seconds(), "sec")
}
