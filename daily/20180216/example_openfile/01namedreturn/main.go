package main

import (
	"errors"
	"fmt"
)

func f() (ERR error) {
	ERR = errors.New("f0")
	return
}

func g() (ERR error) {
	defer func() { ERR = errors.New("g0") }()
	return
}

func h() (ERR error) {
	defer func() { ERR = errors.New("h0") }()
	return nil
}

func i() (ERR error) {
	defer func() { ERR = errors.New("i0") }()
	return errors.New("i1")
}

func j() (ERR error) {
	defer func() {
		if ERR == nil {
			ERR = errors.New("j0")
		}
	}()
	return errors.New("j1")
}

func main() {
	fmt.Println("f", f())
	fmt.Println("g", g())
	fmt.Println("h", h())
	fmt.Println("i", i())
	fmt.Println("j", j())
}
