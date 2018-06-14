package main

import (
	"encoding/csv"
	"flag"
	"fmt"
	"log"
	"os"
	"strings"
	"text/tabwriter"
)

func main() {
	f := flag.String("file", "data", "csv data")
	flag.Parse()

	if err := run(*f); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(filename string) error {
	f, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer f.Close()

	r := csv.NewReader(f)
	records, err := r.ReadAll()
	if err != nil {
		return err
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 4, 4, ' ', 0)
	if len(records) > 0 {
		fmt.Fprintln(w, strings.Join(records[0], "\t"))
		fmt.Fprintln(w, "----------------------------------------")
		for _, row := range records[1:] {
			fmt.Fprintln(w, strings.Join(row, "\t"))
		}
		w.Flush()

	}
	return nil
}
