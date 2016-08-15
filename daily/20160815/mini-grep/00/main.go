// grep content

package main

import (
	"fmt"
	"regexp"
	"strings"
)

func main() {
	rx := regexp.MustCompile("^hello")
	s := strings.TrimSpace(`\
hello.
this is my first code.
hello, hello.

bye.
`)

	for _, line := range strings.Split(s, "\n") {
		if rx.Match([]byte(line)) {
			fmt.Println(line)
		}
	}
}
