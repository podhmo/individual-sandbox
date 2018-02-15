package main

import (
	"fmt"
	"log"

	"golang.org/x/tools/go/loader"
)

func main() {
	conf := loader.Config{
		AllowErrors: true,
	}
	conf.Import("fmt")
	info, err := conf.Load()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(info)
}
