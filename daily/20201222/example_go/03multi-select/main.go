package main

import (
	"fmt"
	"math/rand"
	"reflect"
	"sync"
	"time"
)

func jitter(d time.Duration) time.Duration {
	j := 1.0
	if j < 0.0 {
		return d
	}

	r := rand.Float64() * float64(d)
	if j > 0.0 && j < 1.0 {
		r = float64(j)*r + float64(1.0-j)*float64(d)
	}

	return time.Duration(r)
}
func main() {
	var r [][]int

	chs := []chan int{
		make(chan int),
		make(chan int),
		make(chan int),
		make(chan int),
	}
	ping := func() {
		fmt.Printf("ping	%+v\n", r)
		time.Sleep(1000 * time.Millisecond)
	}
	go func() {
		for {
			ping()
		}
	}()

	var wg sync.WaitGroup
	for _, ch := range chs {
		ch := ch
		wg.Add(1)
		go func() {
			defer wg.Done()
			defer close(ch)

			ch <- 1
			time.Sleep(jitter(100 * time.Millisecond))
			ch <- 2
			time.Sleep(jitter(100 * time.Millisecond))
			ch <- 3
		}()
	}

	c := map[int][]int{}

	wg.Add(1)
	func() {
		defer wg.Done()

		cases := make([]reflect.SelectCase, len(chs))
		for i, ch := range chs {
			cases[i] = reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(ch)}
		}
		for {
			i, x, ok := reflect.Select(cases)
			c[i] = append(c[i], int(x.Int()))
			if !ok {
				cases[i].Chan = reflect.ValueOf(nil)
				fmt.Println("finish", i, c[i])
				r = append(r, c[i])
			}

			fmt.Println("check", len(r))
			if len(r) >= len(chs) {
				fmt.Println("check ok")
				ping()
				return
			}
			time.Sleep(50 * time.Millisecond)
		}
	}()

	wg.Wait()
	fmt.Println("OK")
}
