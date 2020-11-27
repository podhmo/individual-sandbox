package main

import (
	"context"
	"fmt"
	"time"
)

type Person struct {
	Name string `validator:"required"`
}

type Message struct {
	Message string `validator:"required"`
}

func Hello(person Person, message string) Message {
	return Message{
		Message: fmt.Sprintf("%s: Hello, %s", person.Name, message),
	}
}

// generate
//   web: POST /Hello {person: Person, message: string}
//   cli: Hello --person @person.json --message "world"
//   doc: openapi
//   registry: 

// provider (DI)
// cli is string -> primitive -> compoents
// web-API is primitive -> components

// more
//  interceptor:

type Event struct {
	Info   map[string]string
	Params map[string]interface{}
}

func Logging(ctx context.Context, ev Event, next func(ctx context.Context, ev Event) error) error {
	logger := GetLogger(ctx)

	id := ev.Info["requestId"] // TODO: constant?
	st := time.Now()
	logger.Info("start: id=%s", id)
	defer func() {
		logger.Info("  end: id=%s, elappsed: %s", id, st-time.Time())
	}()

	if err := next(ctx, ev); err != nil {
		return err
	}
	return nil
}
