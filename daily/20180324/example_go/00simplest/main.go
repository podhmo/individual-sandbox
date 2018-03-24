package main

import (
	"fmt"
	"time"
)

func main() {
	go func() {
		time.Sleep(100 * time.Millisecond)
		fmt.Println("	one")
	}()
	go func() {
		time.Sleep(100 * time.Millisecond)
		fmt.Println("	two")
	}()
	fmt.Println("wait...")
	time.Sleep(200 * time.Millisecond)
	fmt.Println("done")
}
