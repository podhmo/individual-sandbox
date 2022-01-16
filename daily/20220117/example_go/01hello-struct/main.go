package main

import (
	"fmt"
	"m/structopt"
	"os"
	"time"
)

func main() {
	var options struct {
		Name    string   `json:"name" help:"name of greeting"`
		Age     uint     `json:"age" help:"age of person"`
		Verbose bool     `json:"verbose" short:"v" help:"verbose option"`
		Other   []string `json:"other"`
		Time    time.Duration
	}
	options.Name = "foo"
	options.Other = []string{"xxx", "yyy"}

	b := structopt.NewBuilder()
	b.EnvPrefix = "X_"

	fs := b.Build(&options)
	fs.Parse(os.Args[1:])

	fmt.Printf("%+v\n", options)
}
