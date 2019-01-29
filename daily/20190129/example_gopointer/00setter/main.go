package main

import (
	"fmt"
	"log"
)

func main() {
	p := &p{}
	p.SetName("foo")
	if err := run(p); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(p *p) error {
	fmt.Printf("xxx%qxxx\n", p.Name())
	return nil
}

type p struct {
	Handler
}

// Handler :
type Handler struct {
	name string
}

// SetName is a setter for name option.
func (h *Handler) SetName(name string) {
	h.name = name
}

// Name :
func (h *Handler) Name() string {
	return h.name
}
