package internal

import (
	"errors"
	"fmt"
	"time"
)

type Message string

func NewMessage() Message {
	return Message("Hi there!")
}

func NewGreeter(m Message) Greeter {
	var grumby bool
	if time.Now().Unix()%2 == 0 {
		grumby = true
	}
	return Greeter{Message: m, Grumby: grumby}
}

type Greeter struct {
	Message Message // <- adding a Message field
	Grumby  bool
}

func (g Greeter) Greet() Message {
	return g.Message
}
func NewEvent(g Greeter) (Event, error) {
	if g.Grumby {
		return Event{}, errors.New("could not create event: event greeter is grumpy")
	}
	return Event{Greeter: g}, nil
}

type Event struct {
	Greeter Greeter // <- adding a Greeter field
}

func (e Event) Start() {
	msg := e.Greeter.Greet()
	fmt.Println(msg)
}
