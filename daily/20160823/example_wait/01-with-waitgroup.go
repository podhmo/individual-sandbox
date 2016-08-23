package main

import (
	"log"
	"math/rand"
	"time"
	"fmt"
	"sync"
)

func task(id int, d time.Duration) error {
	log.Printf("id=%2d: start %d", id, d)
	time.Sleep(d)
	log.Printf("id=%2d: end %d", id, d)
	return nil
}

func main() {
    st := time.Now()
    var wg sync.WaitGroup

	for i := 0; i < 100; i++ {
		i := i
        wg.Add(1)
		go func() {
			err := task(i, time.Duration(rand.Intn(1000))*time.Millisecond)
            if err != nil {
                log.Printf("err: %s\n", err)
            }
            wg.Done()
		}()
	}

    wg.Wait()
    fmt.Printf("end with %s\n", time.Now().Sub(st))
}
