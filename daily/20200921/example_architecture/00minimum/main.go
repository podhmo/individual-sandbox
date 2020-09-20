package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Todo struct {
	Title string
	Done  bool
}

var store []*Todo
var Debug = false

func init() {
	store = []*Todo{
		&Todo{Title: "Go to bed", Done: false},
	}
}

func main() {
	r := os.Stdin
	w := os.Stderr
	s := bufio.NewScanner(r)
	s.Split(bufio.ScanLines)
	fmt.Fprintf(w, "> ")

	for s.Scan() {
		line := strings.TrimSpace(s.Text())
		if Debug {
			log.Printf("<- %q\n", line)
		}

		parts := strings.SplitN(line, " ", 2)
		if len(parts) == 1 {
			parts = append(parts, "")
		}

		cmd := parts[0]
		text := parts[1]

		var action func(text string) error
		switch strings.ToLower(cmd) {
		case "help":
			action = Help
		case "list":
			action = List
		case "add":
			action = Add
		case "done":
			action = Done
		default:
			action = Add
			text = cmd
		}

		err := action(text)
		if err != nil {
			log.Println("!!", err)
		}
		if Debug {
			log.Printf("-> %q\n", parts) // response
		}
		fmt.Fprintf(w, "> ")
	}
}

func Help(text string) error {
	fmt.Fprint(os.Stdout, `Avaiable commands:

- add <title> -- add todo
- list        -- list todos

`)
	return nil
}

func List(text string) error {
	if Debug {
		log.Printf("list: len(%d)\n", len(store))
	}

	for i, item := range store {
		if item.Done {
			continue
		}
		fmt.Fprintf(os.Stdout, "\t%02d: %s\n", i, item.Title)
	}
	return nil
}

func Add(text string) error {
	// todo: debug print
	item := &Todo{Title: text}
	log.Printf("add: %#+v\n", item)
	store = append(store, item)
	return nil
}

func Done(text string) error {
	n, err := strconv.ParseInt(text, 10, 64)
	if err != nil {
		return fmt.Errorf("parse: %w", err)
	}

	notFound := fmt.Errorf("not found")
	for i := range store {
		if int64(i) == n {
			store[i].Done = true
			notFound = nil
			log.Printf("done: %#+v\n", store[i])
		}
	}
	return notFound
}
