package main

import (
	"log"
	"math/rand"
	"time"
	"fmt"
)

func task(id int, d time.Duration) error {
	log.Printf("id=%2d: start %d", id, d)
	time.Sleep(d)
	log.Printf("id=%2d: end %d", id, d)
	return nil
}

func main() {
    st := time.Now()
	done := make(chan error, 100)
	for i := 0; i < 100; i++ {
		i := i
		go func() {
			done <- task(i, time.Duration(rand.Intn(1000))*time.Millisecond)
		}()
	}

    for i  := 0; i < 100; i++ {
        err := <- done
		if err != nil {
			log.Printf("err: %s\n", err)
		}
    }
    fmt.Printf("end with %s\n", time.Now().Sub(st))
}
