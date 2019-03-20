package main

import (
	"fmt"
	"strconv"
	"sync"
)

func main() {
	var wg sync.WaitGroup

	N := 20
	chs := make([]<-chan string, N)
	wg.Add(N)

	for i := 0; i < N; i++ {
		i := i
		ch := make(chan string)
		chs[i] = ch
		go func() {
			defer wg.Done()
			defer close(ch)
			for _, x := range []string{
				strconv.Itoa(i) + ":0",
				strconv.Itoa(i) + ":1",
				strconv.Itoa(i) + ":2",
				strconv.Itoa(i) + ":3",
				strconv.Itoa(i) + ":4",
			} {
				ch <- x
				// time.Sleep(10 * time.Millisecond)
			}
		}()
	}

	wg.Add(1)
	go func() {
		defer wg.Done()
		var r []string

		var merge func(chs []<-chan string) <-chan string
		merge = func(chs []<-chan string) <-chan string {
			switch len(chs) {
			case 0:
				return nil
			case 1:
				return chs[0]
			default:
				ch := make(chan string)
				go func() {
					defer close(ch)
					restCH := merge(chs[1:])
					for {
						select {
						case x, ok := <-chs[0]:
							if !ok {
								if restCH == nil {
									return
								}
								for y := range restCH {
									ch <- y
								}
								return
							}
							ch <- x
						case x, ok := <-restCH:
							if !ok {
								if chs[0] == nil {
									return
								}
								for y := range chs[0] {
									ch <- y
								}
								return
							}
							ch <- x
						}
					}
				}()
				return ch
			}
		}
		for x := range merge(chs) {
			fmt.Println(x)
			r = append(r, x)
		}
		fmt.Println(r)
	}()
	wg.Wait()
}
