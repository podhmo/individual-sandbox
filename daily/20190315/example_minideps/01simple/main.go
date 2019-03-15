package main

import (
	"log"
	"m/minideps"

	"github.com/k0kubun/pp"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	deps, start := minideps.New()
	p0 := deps.NewProducer(func(state minideps.State) {
		pp.Println("p0", state)
	})
	p1 := deps.NewProducer(func(state minideps.State) {
		pp.Println("p1", state)
	})
	p2 := deps.NewProducer(func(state minideps.State) {
		pp.Println("p2", state)
	})
	c0 := deps.NewConsumer(func(state minideps.State) {
		pp.Println("c0", state)
	})
	c1 := deps.NewConsumer(func(state minideps.State) {
		pp.Println("c1", state)
	})
	c0(deps.WithDeactivated(), p0, p1)
	c1(deps.WithActivated(), p1, p2)
	start()
	return nil
}
