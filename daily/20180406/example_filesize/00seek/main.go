package main

import (
	"fmt"
	"log"
	"os"
)

func main() {
	f, err := os.Open("testdata/a.txt")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(f.Seek(0, 2))
}
