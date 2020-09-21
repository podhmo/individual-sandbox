package main

import (
	"flag"
	"log"
	"m/04func/internal"
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
	// todo: show help

	r := &internal.Resource{
		Store:  store,
		Output: os.Stdout,
		Debug:  os.Stderr,
	}
	defer func() {
		// xxx: for -save
		store = r.Store
	}()

	h := &internal.Handler{
		Router: &internal.Router{
			Actions: map[string]internal.Action{
				"help": internal.NewHelpC(r),
				"list": internal.NewListC(r, internal.NewList(r)),
				"add":  internal.NewAddC(r, internal.NewAdd(r)),
				"done": internal.NewDoneC(r, internal.NewDone(r)),
			},
		},
		NotFound: func(args []string) error {
			if debug {
				log.Printf("not found: %q", args)
			}
			return internal.NewAddC(r, internal.NewAdd(r))([]string{args[0]})
		},
	}
	if err := h.OnHandle(args); err != nil {
		return err
	}
	return nil
}
