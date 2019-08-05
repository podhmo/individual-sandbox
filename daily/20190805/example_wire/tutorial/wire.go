//+build wireinject

package main

import (
	"m/tutorial/event"
	"m/tutorial/greeter"
	"m/tutorial/message"

	"github.com/google/wire"
)

func InitializeEvent(phrase string) (event.Event, error) {
	wire.Build(event.NewEvent, greeter.NewGreeter, message.NewMessage)
	return event.Event{}, nil
}
