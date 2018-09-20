package main

import (
	"errors"
	"fmt"
	"log"
)

func f(n int) (int, error) {
	if n < 0 {
		return n, errors.New("positive value")
	}
	return n, nil
}

func run(x, y int) (int, error) {
	n, err := f(x)
	if err != nil {
		return 0, err
	}
	m, err2 := f(x)
	if err2 != nil {
		return 0, err2
	}
	return n + m, nil
}

func main() {
	n, err := run(10, 20)
	if err != nil {
		log.Fatalf("%+v", err)
	}
	fmt.Println(n)
}
