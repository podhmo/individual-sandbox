package main

import (
	"fmt"
	"log"
)

func main() {
	err := fmt.Errorf("fmm")
	if err != nil {
		log.Fatal(err)
	}
}
