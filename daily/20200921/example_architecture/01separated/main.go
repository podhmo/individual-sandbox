package main

import (
	"bufio"
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

var store []Todo
var debug = false

func init() {
	store = []Todo{
		Todo{Title: "Go to bed", Done: false},
	}
}

func main() {
	ex := &Executor{
		Debug:  debug,
		Store:  store,
		Writer: os.Stdout,
	}
	h := &Handler{
		Dispatcher: &Dispatcher{
			actions: map[string]Action{
				"help": ex.Help,
				"list": ex.List,
				"add":  ex.Add,
				"done": ex.Done,
			},
		},
		NotFound: func(args []string) error {
			if debug {
				log.Printf("not found: %q", args)
			}
			return ex.Add([]string{args[0]})
		},
	}

	if err := (&Repl{
		Handler: h,
		Debug:   debug,
	}).Start(
		os.Stdin,
		os.Stderr,
	); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

type Repl struct {
	Handler *Handler
	Debug   bool
}

func (repl *Repl) Start(r io.Reader, w io.Writer) error {
	i := 0

	s := bufio.NewScanner(r)
	s.Split(bufio.ScanLines)

	fmt.Fprintf(w, "In [%02d]:", i)
	for s.Scan() {
		i++

		line := strings.TrimSpace(s.Text())
		if repl.Debug {
			log.Printf("<- %q\n", line)
		}

		parts := strings.SplitN(line, " ", 2)
		err := repl.Handler.OnHandle(parts)
		if err != nil {
			return fmt.Errorf("repl: %w", err)
		}
		if repl.Debug {
			log.Printf("-> %q\n", parts)
		}
		fmt.Fprintf(w, "In [%02d]:", i)
	}
	return nil
}

type Dispatcher struct {
	actions map[string]Action
}

var (
	ErrNotFound   = fmt.Errorf("not found")
	ErrUnexpected = fmt.Errorf("unexpected")
)

func (d *Dispatcher) Dispatch(cmd string) (Action, error) {
	action, ok := d.actions[strings.ToLower(cmd)]
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

type Executor struct {
	Store []Todo

	Debug  bool
	Writer io.Writer
}

func (e *Executor) Help(args []string) error {
	fmt.Fprint(os.Stdout, `Avaiable commands:

- list                -- list todos
- add  <title:string> -- add todo
- done <number:int>   -- mark todo as done
`)
	return nil
}

func (e *Executor) List(args []string) error {
	if e.Debug {
		log.Printf("list: len(%d)\n", len(store))
	}

	for i, item := range e.Store {
		if item.Done {
			continue
		}
		fmt.Fprintf(e.Writer, "\t%02d: %s\n", i, item.Title)
	}
	return nil
}

func (e *Executor) Add(args []string) error {
	text := strings.TrimSpace(args[0])
	item := Todo{Title: text}
	fmt.Fprintf(e.Writer, "add: %#+v\n", item)
	e.Store = append(e.Store, item)
	return nil
}

func (e *Executor) Done(args []string) error {
	text := args[0]
	n, err := strconv.ParseInt(text, 10, 64)
	if err != nil {
		return fmt.Errorf("parse: %w", err)
	}

	store := e.Store
	err = ErrNotFound

	r := make([]Todo, len(store))
	for i := range store {
		item := store[i]
		if int64(i) == n {
			item.Done = true
			err = nil
			fmt.Fprintf(e.Writer, "done: %#+v\n", store[i])
		}
		r[i] = item
	}
	e.Store = r // save
	return err
}
