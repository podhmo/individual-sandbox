package greeter

import (
	"m/tutorial/message"
	"time"
)

func NewGreeter(m message.Message) Greeter {
	var grumby bool
	if time.Now().Unix()%2 == 0 {
		grumby = true
	}
	return Greeter{Message: m, Grumby: grumby}
}

type Greeter struct {
	Message message.Message // <- adding a Message field
	Grumby  bool
}

func (g Greeter) Greet() message.Message {
	return g.Message
}
