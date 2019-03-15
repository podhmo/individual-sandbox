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
		pp.Println(state.Name, state.Disabled)
	})
	p1 := deps.NewNode("p1", func(state minideps.State) {
		pp.Println(state.Name, state.Disabled)
	})
	p2 := deps.NewNode("p2", func(state minideps.State) {
		pp.Println(state.Name, state.Disabled)
	})
	c0 := deps.NewNode("c0", func(state minideps.State) {
		pp.Println(state.Name, state.Disabled)
	})
	c1 := deps.NewNode("c1", func(state minideps.State) {
		pp.Println(state.Name, state.Disabled)
	})

	c0.Adjust(deps.WithDepends(p0, p1), deps.WithDisabled())
	c1.Adjust(deps.WithDepends(p1, p2))

	start()
	return nil
}
