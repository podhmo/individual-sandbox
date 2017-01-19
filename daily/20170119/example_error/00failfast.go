package main

import (
	"errors"
	"fmt"
)

func f() ([]bool, error) {
	var r []bool
	ok0 := true
	ok1 := false
	ok2 := true
	if !ok0 {
		return r, errors.New("ng0")
	}
	r = append(r, ok0)
	if !ok1 {
		return r, errors.New("ng1")
	}
	r = append(r, ok1)
	if !ok2 {
		return r, errors.New("ng2")
	}
	r = append(r, ok2)
	return r, nil
}

func main() {
	r, err := f()
	fmt.Println(r, err)
    // [true] ng1
}
