package main

import (
	"errors"
	"fmt"
)

// ParsePositive :
func ParsePositive(n int) (int, error) {
	// 通常は文字列などかもしれない
	if n >= 0 {
		return n, nil
	}
	return n, errors.New("not positive")
}

// Must :
func Must(n int, err error) int {
	if err != nil {
		panic(err)
	}
	return n
}

func main() {
	n := Must(ParsePositive(10))
	fmt.Println("for test ", n)
}
