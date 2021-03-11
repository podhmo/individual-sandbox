package main

import (
	"fmt"
	"os"

	"github.com/mattn/go-isatty"
)

func main() {
	if isatty.IsTerminal(os.Stdin.Fd()) {
		fmt.Println("Is Terminal")
	} else if isatty.IsCygwinTerminal(os.Stdin.Fd()) {
		fmt.Println("Is Cygwin/MSYS2 Terminal")
	} else {
		fmt.Println("Is Not Terminal")
	}
}
