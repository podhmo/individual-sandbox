package main

import (
	"fmt"
	"math/rand"
	"sync"

	"github.com/pkg/errors"
)

// Alternative : alternative error group
type Alternative struct {
	C    chan error
	once sync.Once
	wg   sync.WaitGroup
}

// WaitWith :
func (a *Alternative) WaitWith(fn func()) {
	go fn()
	a.once.Do(func() {
		a.wg.Wait()
		close(a.C)
	})
	a.wg.Wait()
}

// Go :
func (a *Alternative) Go(fn func()) {
	a.wg.Add(1)

	go func() {
		defer a.wg.Done()
		fn()
	}()
}

func ints(a *Alternative, i, n int) chan int {
	ch := make(chan int)
	a.Go(func() {
		defer close(ch)
		for ; i < n; i++ {
			x := rand.Float64()
			if x > 0.8 {
				a.C <- errors.Errorf("xx:%d v=%f", n, x)
				continue
			}
			i := i
			ch <- i
		}
	})
	return ch
}

func mtonpipe(a *Alternative, ns, ms <-chan int) <-chan string {
	ch := make(chan string)
	var wg sync.WaitGroup
	a.Go(func() {
		defer close(ch)

		for i := 0; i < 5; i++ {
			i := i
			wg.Add(1)
			go func() {
				defer wg.Done()
				for n := range ns {
					x := rand.Float64()
					if x > 0.8 {
						a.C <- errors.Errorf("n%d:%d v=%f", i, n, x)
						continue
					}
					ch <- fmt.Sprintf("n%d:%d", i, n)
				}
			}()
		}

		for i := 0; i < 3; i++ {
			i := i
			wg.Add(1)
			go func() {
				defer wg.Done()
				for n := range ms {
					x := rand.Float64()
					if x > 0.8 {
						a.C <- errors.Errorf("m%d:%d v=%f", i, n, x)
						continue
					}
					ch <- fmt.Sprintf("n%d:%d", i, n)
				}
			}()
		}
		wg.Wait()
	})
	return ch
}

func main() {
	a := Alternative{C: make(chan error)}
	ns := ints(&a, 1, 27)
	ms := ints(&a, 1, 15)

	a.Go(func() {
		for x := range mtonpipe(&a, ns, ms) {
			fmt.Println(x)
		}
	})

	a.WaitWith(func() {
		for err := range a.C {
			fmt.Println(err)
		}
	})

	fmt.Println("ok")
}
