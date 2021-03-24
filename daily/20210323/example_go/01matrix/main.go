package main

import (
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	{
		var m [4][4]int
		fmt.Println(m)

	}
	{
		var m [4][4]*int
		fmt.Println(m)
	}
	return nil
}
