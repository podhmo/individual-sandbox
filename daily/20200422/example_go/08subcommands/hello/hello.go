package hello

import (
	"fmt"

	"github.com/integrii/flaggy"
)

var (
	Commandline = flaggy.NewSubcommand("hello")
	Name        string
)

func init() {
	Commandline.Description = "hello message"
	Commandline.String(&Name, "n", "name", "name of person")
}

func Run() error {
	fmt.Println("Hello", Name)
	return nil
}
