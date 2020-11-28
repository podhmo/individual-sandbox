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
	var input struct {
		User  action.User `json:"user" validate:"required"`
		Short bool        `json:"short"`
	}
	if err := tenuki.DecodeJSON(ev.Body, &input); err != nil {
		return nil, apperror.New(err, 400)
	}
	// ?
	short := false
	if ok, err := strconv.ParseBool(ev.Headers.Get("short")); err == nil {
		short = ok
	}
	return action.Hello(ctx, input.User, &short)
}
func IsEven(ctx context.Context, ev Event) (interface{}, error) {
	var input struct {
		V int `json:"v"`
	}
	if err := tenuki.DecodeJSON(ev.Body, &input); err != nil {
		return nil, apperror.New(err, 400)
	}
	return action.IsEven(ctx, input.V)
}

var (
	service = &action.TodoService{Todos: []action.Todo{{Title: "dummy"}}}
)

func AddTodo(ctx context.Context, ev Event) (interface{}, error) {
	var input struct {
		Todo action.Todo `json:"todo" validate:"required"`
	}
	if err := tenuki.DecodeJSON(ev.Body, &input); err != nil {
		return nil, apperror.New(err, 400)
	}
	return service.Add(ctx, input.Todo)
}
func ListTodo(ctx context.Context, ev Event) (interface{}, error) {
	return service.List(ctx)
}

// ---
func Greet(ctx context.Context, ev Event) (interface{}, error) {
	var input struct {
		Name string `json:"name"`
	}
	if err := tenuki.DecodeJSON(ev.Body, &input); err != nil {
		return nil, apperror.New(err, 400)
	}
	registry := action.GetRegistry(ctx)
	return action.Greet(ctx, registry.GetGreeter, input.Name)
}
