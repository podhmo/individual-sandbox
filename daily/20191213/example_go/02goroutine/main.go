package main

import (
	"fmt"
	"log"
	"runtime"
	"sync"
)

func findCaller(depth int) string {
	_, filename, lineno, _ := runtime.Caller(depth)
	return fmt.Sprintf("%d:%s", lineno, filename)
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		foo()
	}()
	wg.Wait()
	return nil
}

func foo() {
	fmt.Println(findCaller(1))
}
