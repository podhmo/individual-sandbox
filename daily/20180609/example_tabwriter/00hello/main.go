package main

import (
	"fmt"
	"log"
	"os"
	"text/tabwriter"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	w := tabwriter.NewWriter(os.Stdout, 0, 4, 4, ' ', 0)
	fmt.Println("四季")
	fmt.Println("----------------------------------------")
	fmt.Fprintln(w, "spring\tsummer\tautumn\twinter")
	fmt.Fprintln(w, "春\t夏\t秋\t冬")
	w.Flush()
	return nil
}
