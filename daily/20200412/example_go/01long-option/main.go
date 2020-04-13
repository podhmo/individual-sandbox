package main

import (
	"flag"
	"fmt"
)

func ResetForTesting(usage func()) {
	flag.CommandLine = flag.NewFlagSet("main", flag.ContinueOnError)
	flag.CommandLine.Usage = usage
}

func main() {
	ResetForTesting(nil)
	n := flag.Int64("int64", 2, "int64 value")

	flag.Parse()
	fmt.Println(n)
}
