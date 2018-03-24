package main

import (
	"fmt"
	"math/rand"
	"sync"

	"github.com/pkg/errors"
)

func ints(errch chan<- error, i, n int) chan int {
	ch := make(chan int)
	go func() {
		defer close(ch)
		for ; i < n; i++ {
			x := rand.Float64()
			if x > 0.8 {
				errch <- errors.Errorf("hmm %d v=%f", n, x)
				continue
			}
			i := i
			ch <- i
		}
	}()
	return ch
}

func mtonpipe(errch chan<- error, ns, ms <-chan int) <-chan string {
	ch := make(chan string)
	go func() {
		var wg sync.WaitGroup
		wg.Add(3)
		defer close(ch)
		go func() {
			defer wg.Done()
			for n := range ns {
				x := rand.Float64()
				if x > 0.95 {
					errch <- errors.Errorf("hmm %d v=%f", n, x)
					continue
				}
				ch <- fmt.Sprintf("n0:%d", n)
			}
		}()
		go func() {
			defer wg.Done()
			for n := range ns {
				x := rand.Float64()
				if x > 0.95 {
					errch <- errors.Errorf("hmm %d v=%f", n, x)
					continue
				}
				ch <- fmt.Sprintf("n1:%d", n)
			}
		}()
		go func() {
			defer wg.Done()
			for n := range ms {
				x := rand.Float64()
				if x > 0.95 {
					errch <- errors.Errorf("hmm %d v=%f", n, x)
					continue
				}
				ch <- fmt.Sprintf("m0:%d", n)
			}
		}()
		wg.Wait()
	}()
	return ch
}

func main() {
	errch := make(chan error)
	ns := ints(errch, 1, 7)
	ms := ints(errch, 1, 5)

	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		for {
			select {
			case err, ok := <-errch:
				if !ok {
					return
				}
				fmt.Println(err, ok)
			}
		}
	}()
	go func() {
		defer close(errch)
		defer wg.Done()
		for x := range mtonpipe(errch, ns, ms) {
			fmt.Println(x)
		}
	}()
	wg.Wait()
	fmt.Println("ok")
}
