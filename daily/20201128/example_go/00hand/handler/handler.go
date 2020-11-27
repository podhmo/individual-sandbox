package handler

import (
	"context"
	"io"
	"m/00hand/action"
	"m/00hand/apperror"
	"strconv"

	"github.com/podhmo/tenuki"
)

type Headers interface {
	Get(string) string
}

type Event struct {
	Name    string
	Headers Headers
	Body    io.ReadCloser
}

type HandlerFunc func(context.Context, Event) (interface{}, error)

type key string

const (
	keyEvent key = ":event"
)

func WithEvent(ctx context.Context, ev Event) context.Context {
	return context.WithValue(ctx, keyEvent, ev)
}

// ----------------------------------------
func Hello(ctx context.Context, ev Event) (interface{}, error) {
	var input action.HelloInput
	if err := tenuki.DecodeJSON(ev.Body, &input); err != nil {
		return nil, apperror.New(err, 400)
	}
	short := false
	if ok, err := strconv.ParseBool(ev.Headers.Get("short")); err == nil {
		short = ok
	}
	return action.Hello(ctx, input, &short)
}
func IsEven(ctx context.Context, ev Event) (interface{}, error) {
	var v int
	if err := tenuki.DecodeJSON(ev.Body, &v); err != nil {
		return nil, apperror.New(err, 400)
	}
	return action.IsEven(ctx, v)
}
