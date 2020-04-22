package bye

import (
	"fmt"

	"github.com/integrii/flaggy"
)

var (
	Commandline = flaggy.NewSubcommand("bye")
	Name        string
)

func init() {
	Commandline.Description = "bye message"
	required := true
	Commandline.AddPositionalValue(&Name, "name", 1, required, "name of person")
}

func Run() error {
	fmt.Println("Bye", Name)
	return nil
}
