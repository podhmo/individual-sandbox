package main

import (
	"log"
	"fmt"
)

func foo(n int) {
	log.Println("----------------------------------------")
	defer log.Println("----------------------------------------")
    log.Println("opening connection...")
	log.Printf("foo: %d\n", n)
    log.Println("closing connection...")
}

func main() {
	for i := 0; i < 5; i++ {
		foo(i)
        fmt.Println("")
	}
}
