package main

import (
	"fmt"
	"log"
	"net/url"
)

func main() {
	parsed, err := url.Parse("http://localhost:9000/hoho")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(parsed.Scheme, parsed.Host, parsed.Hostname())
	// http localhost:9000 localhost
}
