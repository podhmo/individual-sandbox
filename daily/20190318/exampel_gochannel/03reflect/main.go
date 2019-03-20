package main

import (
	"fmt"
	"reflect"
	"sync"
	"time"
)

func main() {
	var wg sync.WaitGroup

	ch0 := make(chan int)
	ch1 := make(chan int)
	ch2 := make(chan int)
	wg.Add(3)
	go func() {
		defer close(ch0)
		defer wg.Done()
		for _, x := range []int{1, 2, 3, 4, 5} {
			ch0 <- x
			time.Sleep(10 * time.Millisecond)
		}
	}()
	go func() {
		defer close(ch1)
		defer wg.Done()
		for _, x := range []int{-1, -2, -3, -4, -5} {
			ch1 <- x
			time.Sleep(20 * time.Millisecond)
		}
	}()
	go func() {
		defer close(ch2)
		defer wg.Done()
		for _, x := range []int{10, 20, 30} {
			ch1 <- x
			time.Sleep(30 * time.Millisecond)
		}
	}()
	wg.Add(1)
	go func() {
		defer wg.Done()
		var r []int

		cases := []reflect.SelectCase{
			reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(ch0)},
			reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(ch1)},
			reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(ch2)},
		}
		for n := 3; n > 0; {
			i, x, ok := reflect.Select(cases)
			if !ok {
				n--
				cases[i].Chan = reflect.ValueOf(nil) // nil channel
				continue
			}
			r = append(r, int(x.Int()))
		}
		fmt.Println(r)
	}()
	wg.Wait()
}
