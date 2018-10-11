package main

import (
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

type p struct {
	x *int
}

func run() error {
	x0 := 10
	x1 := 10
	x2 := 10
	p0 := p{x: &x0}
	p1 := p{x: &x1}
	p2 := p{x: &x2}

	ans := 0
	sump := p{x: &ans}

	*sump.x += *p0.x
	*sump.x += *p1.x
	*sump.x += *p2.x

	fmt.Println(ans, ":", x0, x1, x2)
	return nil
}
