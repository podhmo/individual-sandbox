package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

// PrintFirst prints first line of target file
func PrintFirst(filename string) error {
	if _, err := os.Stat(filename); err != nil {
		return err
	}
	fp, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer fp.Close()
	printFirst(fp, os.Stdout)
	return nil
}

func printFirst(r io.Reader, w io.Writer) {
	sc := bufio.NewScanner(r)
	if sc.Scan() {
		fmt.Fprintf(w, "first line: %s", sc.Text())
	}
}

func main() {
	filename := "./hello.txt"
	err := PrintFirst(filename)
	if err != nil {
		panic(err)
	}
}
