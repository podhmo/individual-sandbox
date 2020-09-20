package main

import (
	"flag"
	"log"
	"m/03multi/internal"
	"os"
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
		if filenameFlag != nil && *filenameFlag != "" {
			if err := loader.Save(*filenameFlag, store); err != nil {
				log.Printf("! %+v", err)
				os.Exit(1)
			}
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
	if err := h.OnHandle(args); err != nil {
		return err
	}

	// xxx: for -save
	store = c.Interactor.Store

	return nil
}
