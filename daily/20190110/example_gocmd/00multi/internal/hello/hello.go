package hello

import (
	"fmt"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

// SetupCommand :
func SetupCommand(app *kingpin.Application) (string, func() error) {
	cmd := app.Command("hello", "hello message")

	verbose := cmd.Flag("verbose", "verbose output").Short('v').Bool()
	name := cmd.Arg("name", "name").Default("john doe").String()

	return cmd.FullCommand(), func() error {
		return Hello(*name, *verbose)
	}
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
