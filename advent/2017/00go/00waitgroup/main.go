package main

import (
	"log"
	"sync"
	"time"
)

func main() {
	var wg sync.WaitGroup
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func(i int) {
			log.Println(">>>", i)
			time.Sleep(1 * time.Second)
			log.Println("<<<", i)
			wg.Done()
		}(i)
	}
	wg.Wait()
	log.Println("end")
}
