package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

var (
	__REQUIRED__ []string
)

func main() {
	// BLOCK: define
	id := flag.String("id", "", "<id>")
	data := flag.String("data", "", "<data>")

	flag.Parse()

	seen := make(map[string]bool)
	flag.Visit(func(f *flag.Flag) { seen[f.Name] = true })
	for _, req := range __REQUIRED__ {
		if !seen[req] {
			log.Printf("missing required -%s argument/flag\n", req)
			os.Exit(2)
		}
	}

	// BLOCK: use
	b, _ := ioutil.ReadFile(*data)
	fmt.Println(*id, string(b))
}
