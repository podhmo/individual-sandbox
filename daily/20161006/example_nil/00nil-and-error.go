package main

import (
	"log"
)

func f() (int, error) {
	return 10, nil
}

func main() {
	x, err := f()
	if err != nil {
		log.Fatal(err)
	}
	log.Println(x)
}
