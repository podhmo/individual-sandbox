package main

import "github.com/k0kubun/pp"

type P map[int]int

func main() {
	p := P(map[int]int{})
	p[10] = 10*10
	pp.Print(p)
}
