package main

import (
	"flag"
	"fmt"
	"os"
)

func main() {
	var options struct {
		Name string
	}

	fs := flag.NewFlagSet("hello", flag.ExitOnError)
	fs.StringVar(&options.Name, "name", "", "name of greeting")
	fs.Parse(os.Args[1:])

	fmt.Printf("%#+v\n", options)
}
