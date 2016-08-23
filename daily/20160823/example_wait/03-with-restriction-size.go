package main

import (
	"fmt"
	"log"
	"math/rand"
	"sync"
	"time"
)

func task(id int, d time.Duration) error {
	log.Printf("id=%2d: start %d", id, d)
	time.Sleep(d)
	log.Printf("id=%2d: end %d", id, d)
	return nil
}

func main() {
	semaphore := make(chan struct{}, 10)

	st := time.Now()
	var wg sync.WaitGroup
	var mx sync.Mutex
	errs := map[int]error{}

	for i := 0; i < 100; i++ {
		i := i
		wg.Add(1)
		go func() {
			semaphore <- struct{}{}
			err := task(i, time.Duration(rand.Intn(100))*time.Millisecond)
			if err != nil {
				mx.Lock()
				errs[i] = err
				mx.Unlock()
			}
			<-semaphore
			wg.Done()
		}()
	}

	wg.Wait()
	for id, v := range errs {
		if v != nil {
			err := errs[id]
			log.Printf("err(%2d): %s\n", id, err)
		}
	}
	fmt.Printf("end with %s\n", time.Now().Sub(st))
}
