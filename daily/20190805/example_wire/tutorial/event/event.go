package event

import (
	"fmt"
	"m/tutorial/greeter"
)

func NewEvent(g greeter.Greeter) Event {
	return Event{Greeter: g}
}

type Event struct {
	Greeter greeter.Greeter // <- adding a Greeter field
}

func (e Event) Start() {
	msg := e.Greeter.Greet()
	fmt.Println(msg)
}
