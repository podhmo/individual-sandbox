package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

func includeText(filename string, substr string) bool {
	if _, err := os.Stat(filename); err != nil {
		return false
	}
	fp, err := os.Open(filename)
	if err != nil {
		return false
	}
	content, err := ioutil.ReadAll(fp)
	if err != nil {
		return false
	}
	return strings.Contains(string(content), substr)
}

func main() {
	fmt.Println(includeText(os.Args[1], os.Args[2]))
}
