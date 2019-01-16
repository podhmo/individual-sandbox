package hello

import (
	"fmt"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

var (
	// Cmd :
	Cmd = kingpin.Command("hello", "hello message")

	verbose = Cmd.Flag("verbose", "verbose output").Short('v').Bool()
	name    = Cmd.Arg("name", "name").Default("john doe").String()
)

// Run :
func Run() error {
	return Hello(*name, *verbose)
}

// Hello :
func Hello(name string, verbose bool) error {
	if verbose {
		fmt.Printf("(%s) Hello\n", name)
	} else {
		fmt.Println("hello")
	}
	return nil
}
