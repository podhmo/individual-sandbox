package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"m/03multi/internal"
	"os"
	"strings"
)

var (
	debug = false
	store []internal.Todo
)

func init() {
	store = []internal.Todo{
		{Title: "Go to bed", Done: false},
	}
}

func main() {
	filenameFlag := flag.String("store", "", "store file name (e.g. store.json)")
	saveFlag := flag.Bool("save", false, "save")
	flag.Parse()

	loader := &internal.Loader{}
	if filenameFlag != nil && *filenameFlag != "" {
		if err := loader.Load(*filenameFlag, &store); err != nil {
			log.Printf("! %+v", err)
			os.Exit(1)
		}
	}

	args := flag.Args()
	if err := run(args); err != nil {
		log.Fatalf("!! %+v", err)
	}

	if saveFlag != nil && *saveFlag {
		filename := "x.json"
		if filenameFlag != nil && *filenameFlag != "" {
			filename = *filenameFlag
		}

		if err := loader.Save(filename, store); err != nil {
			log.Printf("! %+v", err)
			os.Exit(1)
		}
	}
}

func run(args []string) error {
	c := &internal.Controller{
		Interactor: &internal.Interactor{
			Debug:  debug,
			Writer: os.Stderr,
			Store:  store,
		},
		Writer: os.Stdout,
	}

	// todo: show help
	h := &internal.Handler{
		Dispatcher: &internal.Dispatcher{
			Actions: map[string]internal.Action{
				"help": c.HelpC,
				"list": c.ListC,
				"add":  c.AddC,
				"done": c.DoneC,
			},
		},
		NotFound: func(args []string) error {
			if debug {
				log.Printf("not found: %q", args)
			}
			return c.AddC([]string{args[0]})
		},
	}
	if err := (&Repl{
		Handler: h,
		Debug:   debug,
	}).Start(
		os.Stdin,
		os.Stderr,
	); err != nil {
		return err
	}

	// xxx: for -save
	store = c.Interactor.Store

	return nil
}

type Repl struct {
	Handler *internal.Handler
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
