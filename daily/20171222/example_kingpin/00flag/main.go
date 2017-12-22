package main

import (
	"fmt"
	"os"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

func run(name string, withname bool) {
	if withname {
		fmt.Println(name, "hello")
	} else {
		fmt.Println("hello")
	}
}

func main() {
	app := kingpin.New("hello", "hello")
	name := app.Arg("name", "name").String()
	withname := app.Flag("with-name", "with name").Bool()

	if _, err := app.Parse(os.Args[1:]); err != nil {
		app.FatalUsage("invalid arguments")
	}
	run(*name, *withname)
}
