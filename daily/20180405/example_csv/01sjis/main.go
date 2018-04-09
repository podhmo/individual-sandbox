package main

import (
	"encoding/csv"
	"fmt"
	"io"
	"log"
	"os"
	"unicode/utf8"
)

func main() {
	f, err := os.Open("testdata/sjis.csv")
	if err != nil {
		log.Fatal(err)
	}
	r := csv.NewReader(f)
	for {
		record, err := r.Read()
		if err == io.EOF {
			break
		}
		fmt.Println(record, utf8.ValidString(record[0]))
	}
}
