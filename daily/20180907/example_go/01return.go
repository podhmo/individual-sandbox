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
	n, xErr := f(x)
	{
		err := xErr
		if err != nil {
			return 0, err
		}
	}
	m, yErr := f(x)
	{
		err := yErr
		if err != nil {
			return 0, err
		}
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
