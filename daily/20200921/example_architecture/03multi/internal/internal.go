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

type Dispatcher struct {
	Actions map[string]Action
}

func (d *Dispatcher) Dispatch(cmd string) (Action, error) {
	action, ok := d.Actions[strings.ToLower(cmd)]
	if !ok {
		return nil, fmt.Errorf("dispatch %q: %w", cmd, ErrNotFound)
	}
	return action, nil
}

type Action func(args []string) error

type Handler struct {
	Dispatcher *Dispatcher
	Handle     func([]string) error
	NotFound   func([]string) error
}

func (h *Handler) OnHandle(args []string) error {
	if len(args) == 1 {
		args = append(args, "")
	}

	if h.Handle != nil {
		return h.Handle(args)
	}
	action, err := h.Dispatcher.Dispatch(args[0])
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

type Controller struct {
	Interactor *Interactor
	Writer     io.Writer
}

func (c *Controller) HelpC(args []string) error {
	fmt.Fprint(os.Stdout, `Avaiable commands:

- list                -- list todos
- add  <title:string> -- add todo
- done <number:int>   -- mark todo as done
`)
	return nil
}

func (c *Controller) ListC(args []string) error {
	items, err := c.Interactor.List()
	if err != nil {
		return fmt.Errorf("listc: %w", err)
	}
	for i, item := range items {
		fmt.Fprintf(c.Writer, "\t%02d: %s\n", i, item.Title)
	}
	return nil
}

func (c *Controller) AddC(args []string) error {
	text := strings.TrimSpace(args[0])
	item := Todo{Title: text}

	_, err := c.Interactor.Add(item)
	if err != nil {
		return fmt.Errorf("addc: %w", err)
	}
	return nil
}

func (c *Controller) DoneC(args []string) error {
	text := args[0]
	n, err := strconv.ParseInt(text, 10, 64)
	if err != nil {
		return fmt.Errorf("parse: %w", err)
	}

	_, err = c.Interactor.Done(int(n))
	if err != nil {
		return fmt.Errorf("donec: %w", err)
	}
	return nil
}

type Interactor struct {
	Store []Todo

	Debug  bool
	Writer io.Writer // debug print
}

func (ir *Interactor) List() ([]Todo, error) {
	if ir.Debug {
		log.Printf("list: len(%d)\n", len(ir.Store))
	}

	var r []Todo
	for _, item := range ir.Store {
		if item.Done {
			continue
		}
		r = append(r, item)
	}
	return r, nil
}

func (ir *Interactor) Add(item Todo) (Todo, error) {
	ir.Store = append(ir.Store, item)
	fmt.Fprintf(ir.Writer, "add: %#+v\n", item)
	return item, nil
}

func (ir *Interactor) Done(n int) ([]Todo, error) {
	store := ir.Store

	r := make([]Todo, len(store))
	found := false
	for i := range store {
		item := store[i]
		if i == n {
			item.Done = true
			found = true
			fmt.Fprintf(ir.Writer, "done: %#+v\n", store[i])
			r[i] = item
			break
		}
		r[i] = item
	}
	if !found {
		return ir.Store, fmt.Errorf("done: %w", ErrNotFound)
	}

	ir.Store = r // save
	return r, nil
}
