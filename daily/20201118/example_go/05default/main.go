package main

import (
	"flag"
	"fmt"
	"m/05default/internal"
	_ "m/05default/issue"
	_ "m/05default/foo"
	"os"
)

func main() {
	internal.MainCLI(
		internal.BuildCommand(os.Args[0]),
		os.Args[1:],
	)
}
