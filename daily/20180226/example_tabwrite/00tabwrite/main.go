package main

import (
	"fmt"
	"os"
	"text/tabwriter"
)

func main() {
	minwidth := 0
	tabwidth := 0
	padding := 2
	padchar := byte('.')
	flags := tabwriter.AlignRight | tabwriter.Debug
	w := tabwriter.NewWriter(os.Stdout, minwidth, tabwidth, padding, padchar, flags)
	fmt.Fprintln(w, "a	b	c")
	fmt.Fprintln(w, "a	bbbbb		c")
	w.Flush()
}
