package main

import (
	"fmt"
	"strings"
)

type W struct {
	P
}

func (w W) Body() string {
	return strings.Repeat(w.P.Body, 2)
}

type P struct {
	Body string
}

func main() {
	w := W{P: P{Body: "hekeheke"}}
	fmt.Println(w.Body())
}
