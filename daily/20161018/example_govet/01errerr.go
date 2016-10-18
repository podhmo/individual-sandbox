package main

import (
	"errors"
	"log"
)

func returnError() (interface{}, error) {
	return nil, errors.New("oops")
}

func main() {
	x, err := returnError()

	// too bad!!
	if err != err {
		log.Println("hmm:", err)
	}

	log.Println("don't call", x)
}
