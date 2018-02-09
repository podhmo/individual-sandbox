package main

import (
	"fmt"
	"strings"
)

// NormalizeURL normalizes url
func NormalizeURL(url string) string {
	// https://foo.bar.jp == foo.bar.jp == foo.bar.jp/
	skipPrefix := []string{"https://", "http://"}
	for _, prefix := range skipPrefix {
		if strings.HasPrefix(url, prefix) {
			url = url[len(prefix):]
		}
	}
	return strings.TrimRight(url, "/")
}

func main() {
	s := "https://wacul-ai.com"
	fmt.Println(NormalizeURL(s))
}
