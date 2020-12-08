package main

import (
	"fmt"
	"strconv"
	"strings"
)

func main() {
	xs := []string{`"foo"`, `"bar"`, `boo`}
	largest := ""
	for _, x := range xs {
		x, err := unescape(x)
		if err != nil {
			panic(err)
		}
		if len(x) < len(largest) {
			continue
		}
		largest = x
		fmt.Println("@", largest)
	}
	fmt.Println("largest:", largest) // ?
}

func unescape(s string) (string, error) {
	if strings.HasPrefix(s, `"`) && strings.HasSuffix(s, `"`) {
		return strconv.Unquote(s)
	}
	return s, nil
}
