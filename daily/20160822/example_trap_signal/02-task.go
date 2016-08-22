package main

import (
	"fmt"
	"sync"
	"time"
)

func task(cb func()) {
	fmt.Println("*task prepare(1s)")
	{
		c0 := time.After(time.Duration(1) * time.Second)
		t := <-c0
		fmt.Printf("*task start(2s) -- %s\n", t)
	}
	{
		c1 := time.After(time.Duration(2) * time.Second)
		t := <-c1
		fmt.Printf("*task end -- %s\n", t)
		cb()
	}
}

func main() {
	for i := 0; i < 2; i++ {
		var wg sync.WaitGroup
		wg.Add(1)
		go task(func() { wg.Done() })
		wg.Wait()
		fmt.Println("----------------------------------------")
	}
	fmt.Println("ok")
}
