package main

import (
	"fmt"
	"os"

	"github.com/spf13/pflag"
)

func main() {
	defaults := pflag.NewFlagSet("defaults for all commands", pflag.ExitOnError)
	forAll := defaults.String("all", "visible for all commands", "visible for all commands")

	cmdA := pflag.NewFlagSet("cmda", pflag.ExitOnError)
	cmdAspecific := cmdA.String("aspec", "only visible for cmd A", "only visible for cmd A")
	cmdA.AddFlagSet(defaults)

	cmdB := pflag.NewFlagSet("cmdb", pflag.ExitOnError)
	cmdBspecific := cmdA.String("bspec", "only visible for cmd B", "only visible for cmd B")
	cmdB.AddFlagSet(defaults)

	defaults.Parse(os.Args)

	// if len(os.Args) == 1 {
	// 	log.Fatal("no subcommand given")
	// }

	switch os.Args[1] {
	case "cmda":
		cmdA.Parse(os.Args[2:])
		fmt.Println(*cmdAspecific)
		fmt.Println(*forAll)
	case "cmdb":
		cmdB.Parse(os.Args[2:])
		fmt.Println(*cmdBspecific)
		fmt.Println(*forAll)
	default:
		fmt.Printf("%q is no valid subcommand.\n", os.Args[1])
		os.Exit(2)
	}
}
