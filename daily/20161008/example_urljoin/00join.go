package main

import (
	"fmt"
	"net/url"
	"path"
)

func main() {
	baseURL, _ := url.Parse("http://foo.bar.jp/boo/?greeting=hello")

	copiedURL := *baseURL
	copiedURL.Path = path.Join(copiedURL.Path, "./bee")

	fmt.Printf("before: %s\n", baseURL)
	fmt.Printf("after : %s\n", &copiedURL)
}
