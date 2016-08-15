package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"regexp"
)

// MiniGrep is mini version of grep command
func MiniGrep(rx *regexp.Regexp, prefix string, r io.Reader) error {
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		if err := scanner.Err(); err != nil {
			return err
		}
		line := scanner.Text()
		if rx.MatchString(line) {
			if prefix == "" {
				fmt.Println(line)
			} else {
				fmt.Printf("%s: %s\n", prefix, line)
			}
		}
	}
	return nil
}

// MiniGrepFile is a variant of MiniGrep().
func MiniGrepFile(rx *regexp.Regexp, prefix string, filename string) error {
	fp, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer fp.Close()
	return MiniGrep(rx, prefix, fp)
}

// high order function. this is not-good example?

// IterateSimplest is iteration strategy for simplest iteration
func IterateSimplest(filenames []string, consume func(filename string) error) {
	for _, filename := range filenames {
		if _, err := os.Stat(filename); err != nil {
			break
		}
		err := consume(filename)
		if err != nil {
			panic(err)
		}
	}
}

func recFiles(filenames []string, consume func(filename string) error) {
	for _, filename := range filenames {
		finfo, err := os.Stat(filename)
		if err != nil {
			panic(err)
		}
		if finfo.IsDir() {
			// W: ignore error
			recDir(filename, consume)
		} else {
			err := consume(filename)
			if err != nil {
				panic(err)
			}
		}
	}
}

func recDir(root string, consume func(filename string) error) error {
	children, err := ioutil.ReadDir(root)
	if err != nil {
		return err
	}
	subFiles := make([]string, 0, len(children))
	for _, finfo := range children {
		subFiles = append(subFiles, path.Join(root, finfo.Name()))
	}
	recFiles(subFiles, consume)
	return nil
}

// IterateRecursively is iteration strategy for searching recursively.
func IterateRecursively(filenames []string, consume func(filename string) error) {
	recFiles(filenames, consume)
}

var recursive = flag.Bool("recursive", false, "recursively search")

func init() {
	flag.BoolVar(recursive, "r", false, "recursively search")
}

func main() {
	flag.Parse()
	args := flag.Args()

	switch {
	case len(args) < 1:
		fmt.Fprintln(os.Stderr, "Mini-grep <regexp> [filename] ...")
	case len(args) == 1:
		rx := regexp.MustCompile(args[0])
		err := MiniGrep(rx, "", os.Stdin)
		if err != nil {
			panic(err)
		}
	default:
		rx := regexp.MustCompile(args[0])
		consume := func(filename string) error {
			return MiniGrepFile(rx, filename, filename)
		}
		if *recursive {
			IterateRecursively(args[1:], consume)
		} else {
			IterateSimplest(args[1:], consume)
		}
	}
}
