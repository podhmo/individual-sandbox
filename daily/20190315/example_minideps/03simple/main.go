package main

import (
	"log"
	minideps "m/minideps2"

	"github.com/k0kubun/pp"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	deps, start := minideps.New()
	p0 := deps.NewNode("p0", func(state minideps.State) {
		pp.Println("p0", state)
	})
	p1 := deps.NewNode("p1", func(state minideps.State) {
		pp.Println("p1", state)
	})
	p2 := deps.NewNode("p2", func(state minideps.State) {
		pp.Println("p2", state)
	})
	c0 := deps.NewNode("c0", func(state minideps.State) {
		pp.Println("c0", state)
	}, p0, p1)
	c1 := deps.NewNode("c1", func(state minideps.State) {
		pp.Println("c1", state)
	}, p1, p2)

	c0.Disabled()
	_ = c1
	start()
	return nil
}
