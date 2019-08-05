//+build wireinject

package main

import (
	"github.com/google/wire"
)

func InitializeEvent() (Event, error) {
	wire.Build(NewEvent, NewGreeter, NewMessage)
	return Event{}, nil
}
