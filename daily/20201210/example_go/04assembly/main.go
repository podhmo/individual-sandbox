// +build ignore

package main

import (
	"m"
	"net/url"
)

func main() {
	v, _ := url.ParseQuery("name=foo")
	m.PrintIfGet(v, "name")
}
