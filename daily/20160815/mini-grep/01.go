package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
)

func miniGrep(rx *regexp.Regexp, filename string) error {
	fp, err := os.Open(filename)
	if err != nil {
		return err
	}

	defer fp.Close()
	scanner := bufio.NewScanner(fp)
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

func main() {
	if len(os.Args) <= 2 {
		fmt.Fprintln(os.Stderr, "mini-grep <regexp> [filename] ...")
	} else {
		fmt.Println(os.Args)
		rx := regexp.MustCompile(os.Args[1])
		for _, filename := range os.Args[2:] {
			err := miniGrep(rx, filename)
			if err != nil {
				panic(err)
			}
		}
	}
}
