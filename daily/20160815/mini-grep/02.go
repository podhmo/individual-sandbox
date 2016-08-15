package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"regexp"
)

func miniGrep(rx *regexp.Regexp, r io.Reader) error {
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		if err := scanner.Err(); err != nil {
			return err
		}
		line := scanner.Text()
		if rx.MatchString(line) {
			fmt.Println(line)
		}
	}
	return nil
}

func miniGrepFile(rx *regexp.Regexp, filename string) error {
	fp, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer fp.Close()
	return miniGrep(rx, fp)
}

func main() {
	switch {
	case len(os.Args) < 2:
		fmt.Fprintln(os.Stderr, "mini-grep <regexp> [filename] ...")
	case len(os.Args) == 2:
		rx := regexp.MustCompile(os.Args[1])
		err := miniGrep(rx, os.Stdin)
		if err != nil {
			panic(err)
		}
	default:
		rx := regexp.MustCompile(os.Args[1])
		for _, filename := range os.Args[2:] {
            if _, err := os.Stat(filename); err != nil {
                break
            }
			err := miniGrepFile(rx, filename)
			if err != nil {
				panic(err)
			}
		}
	}
}
