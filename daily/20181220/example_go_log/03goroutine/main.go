package main

import (
	"log"
	"sync"
)

func main() {
    log.SetFlags(log.Lmicroseconds | log.Lshortfile)
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	var wg sync.WaitGroup
	for i := 0; i < 5; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			log.Println("hello")
		}()
	}
	wg.Wait()
	return nil
}
