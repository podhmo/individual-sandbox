package main

import "errors"

func run() (err error) {
	println("hello")
	return errors.New("x")
}

func main() {
	if err := run(); err != nil {
		println("!! %+v", err)
	}
}
