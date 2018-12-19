package main

import (
	"io"
	"log"
	"os"
	"runtime/debug"
	"strings"
	"sync"
)

type customWriter struct {
	out io.Writer
}

func (w *customWriter) Write(p []byte) (int, error) {
	// goroutine <id> [running]:
	firstline := []byte(strings.SplitN(string(debug.Stack()), "\n", 2)[0])
	return w.out.Write(append(firstline[:len(firstline)-10], p...))
}

func main() {
	log.SetOutput(&customWriter{out: os.Stderr})
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
