package main

import (
	"log"
	"m/myapi"
	"os"
)

func main() {
	url := os.Args[1]
	log.Fatalf("!%+v", myapi.Run(url, myapi.Handler()))
}
