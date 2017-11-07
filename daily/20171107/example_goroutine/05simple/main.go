package main

import (
	"log"
	"strings"
	"sync"
	"time"
)

func run(i int) error {
	log.Printf("-> %s%3d\n", strings.Repeat(" ", i), i)
	time.Sleep(1 * time.Second)
	log.Printf("<- %s%3d\n", strings.Repeat(" ", i), i)
	return nil
}

func main() {
	ch := make(chan struct{}, 4)
	var rErr error
	var errOnce sync.Once
	var wg sync.WaitGroup
	for i := 0; i < 10; i++ {
		i := i
		wg.Add(1)
		go func() {
			ch <- struct{}{}
			err := run(i)
			<-ch
			wg.Done()
			if err != nil {
				errOnce.Do(func() {
					rErr = err
				})
			}
		}()
	}
	wg.Wait()
	if rErr != nil {
		log.Fatal(rErr)
	}
}
