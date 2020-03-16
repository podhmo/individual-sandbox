package main

import (
	"net/url"
	"os"

	"github.com/k0kubun/pp"
)

type Result struct {
	Input  string
	Output *url.URL
}

func main() {
	pp.ColoringEnabled = false
	for _, x := range os.Args[1:] {
		u, err := url.Parse(x)
		if err != nil {
			panic(err)
		}
		pp.Println(Result{
			Input:  x,
			Output: u,
		})
	}
}
