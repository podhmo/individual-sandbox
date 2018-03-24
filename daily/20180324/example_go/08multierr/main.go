package main

import (
	"fmt"
	"math/rand"
	"sync"

	"github.com/pkg/errors"
)

func main() {
	var wg sync.WaitGroup
	var m sync.Mutex
	var errs []error

	wg.Add(100)
	for i := 0; i < 100; i++ {
		i := i
		go func() {
			defer wg.Done()
			v := rand.Float64()
			if v > 0.95 {
				m.Lock()
				errs = append(errs, errors.Errorf("invalid %f (%d)", v, i))
				m.Unlock()
			}
		}()
	}
	wg.Wait()

	for _, err := range errs {
		fmt.Println(err)
	}
}
