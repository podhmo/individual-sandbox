package main

import (
	"errors"
	"log"
	"os"
)

func returnError() (interface{}, error) {
	return nil, errors.New("oops")
}

func main() {
	x, err := returnError()

	// too bad!!
	if err != err {
		log.Println("hmm:", err)
		os.Exit(0)
	}

	log.Println("don't call", x)
}
