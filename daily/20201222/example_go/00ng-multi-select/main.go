package main

import (
	"fmt"
	"sync"
	"time"
)

func main() {
	ob := struct {
		ch0 chan int
		ch1 chan int
		v0  []int
		v1  []int
	}{
		ch0: make(chan int),
		ch1: make(chan int),
	}

	ping := func() {
		fmt.Printf("ping	%+v\n", ob)
		time.Sleep(1000 * time.Millisecond)
	}
	go func() {
		for {
			ping()
		}
	}()

	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		ch := ob.ch0
		defer close(ch)

		ch <- 1
		time.Sleep(100 * time.Millisecond)
		ch <- 2
		time.Sleep(100 * time.Millisecond)
		ch <- 3
	}()

	wg.Add(1)
	go func() {
		defer wg.Done()
		ch := ob.ch1
		defer close(ch)

		ch <- 1
		time.Sleep(100 * time.Millisecond)
		ch <- 2
		time.Sleep(100 * time.Millisecond)
		ch <- 3
	}()

	wg.Add(1)
	go func() {
		defer wg.Done()
		c := 0
		for {
			select {
			case x, ok := <-ob.ch0:
				ob.v0 = append(ob.v0, x)
				if !ok {
					c++
					fmt.Println("finish 0:", ob.v1)
				}
			case x, ok := <-ob.ch1:
				ob.v1 = append(ob.v1, x)
				if !ok {
					c++
					fmt.Println("finish 1:", ob.v1)
				}
			}

			fmt.Println("check", c)
			if c >= 2 {
				fmt.Println("check ok")
				ping()
				break
			}
		}
	}()

	wg.Wait()
	fmt.Println("OK")
}
