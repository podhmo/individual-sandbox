package main

import (
	"errors"
	"log"
)

func somethingWrong() (interface{}, error) {
	return nil, errors.New("oops")
}

func main() {
	x, err := somethingWrong()
	// ./00nilnil.go:14: invalid operation: nil != nil (operator != not defined on nil)
	if nil != nil {
		log.Println("hmm:", err)
	}
}
