package internal

import (
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

type Todo struct {
	Title string
	Done  bool
}

var (
	ErrNotFound   = fmt.Errorf("not found")
	ErrUnexpected = fmt.Errorf("unexpected")
)

type Router struct {
	Actions map[string]Action
}

func (d *Router) Route(cmd string) (Action, error) {
	action, ok := d.Actions[strings.ToLower(cmd)]
	if !ok {
		return nil, fmt.Errorf("dispatch %q: %w", cmd, ErrNotFound)
	}
	return action, nil
}

type Action func(args []string) error

type Handler struct {
	Router   *Router
	Handle   func([]string) error
	NotFound func([]string) error
}

func (h *Handler) OnHandle(args []string) error {
	if len(args) == 1 {
		args = append(args, "")
	}

	if h.Handle != nil {
		return h.Handle(args)
	}
	action, err := h.Router.Route(args[0])
	if err != nil {
		if errors.Is(err, ErrNotFound) {
			return h.OnNotFound(args)
		}
		return fmt.Errorf("handle: %w", ErrUnexpected)
	}
	return action(args[1:])
}
func (h *Handler) OnNotFound(args []string) error {
	if h.NotFound == nil {
		return fmt.Errorf("dispatch %q: %w", args[0], ErrNotFound)
	}
	return h.NotFound(args)
}

type Resource struct {
	Store []Todo

	Debug  io.Writer // debug print
	Output io.Writer // output
}

func NewHelpC(r *Resource) Action {
	return func(args []string) error {
		fmt.Fprint(os.Stdout, `Avaiable commands:

- list                -- list todos
- add  <title:string> -- add todo
- done <number:int>   -- mark todo as done
`)
		return nil
	}
}

func NewListC(r *Resource, interact func() ([]Todo, error)) Action {
	return func(args []string) error {
		items, err := interact()
		if err != nil {
			return fmt.Errorf("listc: %w", err)
		}
		for i, item := range items {
			fmt.Fprintf(r.Output, "\t%02d: %s\n", i, item.Title)
		}
		return nil
	}
}

func NewAddC(r *Resource, interact func(Todo) (Todo, error)) Action {
	return func(args []string) error {
		text := strings.TrimSpace(args[0])
		item := Todo{Title: text}

		_, err := interact(item)
		if err != nil {
			return fmt.Errorf("addc: %w", err)
		}
		return nil
	}
}

func NewDoneC(r *Resource, interact func(int) ([]Todo, error)) Action {
	return func(args []string) error {
		text := args[0]
		n, err := strconv.ParseInt(text, 10, 64)
		if err != nil {
			return fmt.Errorf("parse: %w", err)
		}

		_, err = interact(int(n))
		if err != nil {
			return fmt.Errorf("donec: %w", err)
		}
		return nil
	}
}

func NewList(r *Resource) func() ([]Todo, error) {
	return func() ([]Todo, error) {
		if r.Debug != nil {
			log.Printf("list: len(%d)\n", len(r.Store))
		}

		var newItems []Todo
		for _, item := range r.Store {
			if item.Done {
				continue
			}
			newItems = append(newItems, item)
		}
		return newItems, nil
	}
}

func NewAdd(r *Resource) func(item Todo) (Todo, error) {
	return func(item Todo) (Todo, error) {
		r.Store = append(r.Store, item)

		fmt.Fprintf(r.Output, "add: %#+v\n", item)
		return item, nil
	}
}

func NewDone(r *Resource) func(n int) ([]Todo, error) {
	return func(n int) ([]Todo, error) {
		store := r.Store

		newItems := make([]Todo, len(store))
		found := false
		for i := range store {
			item := store[i]
			if i == n {
				item.Done = true
				found = true
				fmt.Fprintf(r.Output, "done: %#+v\n", store[i])
			}
			newItems[i] = item
		}
		if !found {
			return r.Store, fmt.Errorf("done: %w", ErrNotFound)
		}

		r.Store = newItems // save
		return newItems, nil
	}
}
