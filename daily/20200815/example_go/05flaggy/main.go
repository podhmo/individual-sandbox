package main

import (
	"fmt"
	"os"

	"github.com/integrii/flaggy"
)

func main() {
	flaggy.DebugMode = true

	// Declare variables and their defaults
	var stringFlagF = "defaultValueF"
	var intFlagT = 3
	var boolFlagB bool

	// add a global bool flag for fun
	flaggy.Bool(&boolFlagB, "y", "yes", "A sample boolean flag")
	flaggy.AddPositionalValue(&stringFlagF, "xxx", 1 /* required*/, true, "this is xxx")

	// Parse everything, then use the flags and trailing arguments
	err := flaggy.DefaultParser.Parse()
	if err != nil {
		fmt.Println(err)
		os.Exit(0)
	}
	fmt.Println(stringFlagF)
	fmt.Println(intFlagT)
	fmt.Println(boolFlagB)
}
