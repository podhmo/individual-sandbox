package main

import (
	"os"

	"github.com/k0kubun/pp"
)

func main() {
	fp, err := os.Open("hmm")
	pp.Println(fp)
	pp.Println(err)

	x, err := os.Stat("hmm")
	pp.Println(x)
	pp.Println(err)
}
