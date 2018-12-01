// +build ignore

package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"
)

func main() {
	// These functions are local variables to avoid name conflicts with
	// magefiles.
	list := func() error {

		w := tabwriter.NewWriter(os.Stdout, 0, 4, 4, ' ', 0)
		fmt.Println("Targets:")
		fmt.Fprintln(w, "  build\t"+"A build step that requires additional params, or platform specific steps for example")
		fmt.Fprintln(w, "  clean\t"+"up after yourself")
		fmt.Fprintln(w, "  install\t"+"A custom install step if you need your bin someplace other than go/bin")
		fmt.Fprintln(w, "  installDeps\t"+"Manage your deps, or running package managers.")
		fmt.Fprintln(w, "  ls\t"+"")
		err := w.Flush()
		return err
	}

	var ctx context.Context
	var ctxCancel func()

	getContext := func() (context.Context, func()) {
		if ctx != nil {
			return ctx, ctxCancel
		}

		if os.Getenv("MAGEFILE_TIMEOUT") != "" {
			timeout, err := time.ParseDuration(os.Getenv("MAGEFILE_TIMEOUT"))
			if err != nil {
				fmt.Printf("timeout error: %v\n", err)
				os.Exit(1)
			}

			ctx, ctxCancel = context.WithTimeout(context.Background(), timeout)
		} else {
			ctx = context.Background()
			ctxCancel = func() {}
		}
		return ctx, ctxCancel
	}

	runTarget := func(fn func(context.Context) error) interface{} {
		var err interface{}
		ctx, cancel := getContext()
		d := make(chan interface{})
		go func() {
			defer func() {
				err := recover()
				d <- err
			}()
			err := fn(ctx)
			d <- err
		}()
		select {
		case <-ctx.Done():
			cancel()
			e := ctx.Err()
			fmt.Printf("ctx err: %v\n", e)
			return e
		case err = <-d:
			cancel()
			return err
		}
	}
	// This is necessary in case there aren't any targets, to avoid an unused
	// variable error.
	_ = runTarget

	handleError := func(logger *log.Logger, err interface{}) {
		if err != nil {
			logger.Printf("Error: %v\n", err)
			type code interface {
				ExitStatus() int
			}
			if c, ok := err.(code); ok {
				os.Exit(c.ExitStatus())
			}
			os.Exit(1)
		}
	}
	_ = handleError

	log.SetFlags(0)
	verbose, _ := strconv.ParseBool(os.Getenv("MAGEFILE_VERBOSE"))
	if !verbose {
		log.SetOutput(ioutil.Discard)
	}
	logger := log.New(os.Stderr, "", 0)
	if ok, _ := strconv.ParseBool(os.Getenv("MAGEFILE_LIST")); ok {
		if err := list(); err != nil {
			log.Println(err)
			os.Exit(1)
		}
		return
	}

	targets := map[string]bool{

		"build":       true,
		"clean":       true,
		"install":     true,
		"installdeps": true,
		"ls":          true,
	}

	var unknown []string
	for _, arg := range os.Args[1:] {
		if !targets[strings.ToLower(arg)] {
			unknown = append(unknown, arg)
		}
	}
	if len(unknown) == 1 {
		logger.Println("Unknown target specified:", unknown[0])
		os.Exit(2)
	}
	if len(unknown) > 1 {
		logger.Println("Unknown targets specified:", strings.Join(unknown, ", "))
		os.Exit(2)
	}

	if help, _ := strconv.ParseBool(os.Getenv("MAGEFILE_HELP")); help {
		if len(os.Args) < 2 {
			logger.Println("no target specified")
			os.Exit(1)
		}
		switch strings.ToLower(os.Args[1]) {
		case "build":
			fmt.Print("mage build:\n\n")
			fmt.Println("A build step that requires additional params, or platform specific steps for example")
			fmt.Println()

			var aliases []string
			if len(aliases) > 0 {
				fmt.Printf("Aliases: %s\n\n", strings.Join(aliases, ", "))
			}
			return
		case "clean":
			fmt.Print("mage clean:\n\n")
			fmt.Println("Clean up after yourself")
			fmt.Println()

			var aliases []string
			if len(aliases) > 0 {
				fmt.Printf("Aliases: %s\n\n", strings.Join(aliases, ", "))
			}
			return
		case "install":
			fmt.Print("mage install:\n\n")
			fmt.Println("A custom install step if you need your bin someplace other than go/bin")
			fmt.Println()

			var aliases []string
			if len(aliases) > 0 {
				fmt.Printf("Aliases: %s\n\n", strings.Join(aliases, ", "))
			}
			return
		case "installdeps":
			fmt.Print("mage installdeps:\n\n")
			fmt.Println("Manage your deps, or running package managers.")
			fmt.Println()

			var aliases []string
			if len(aliases) > 0 {
				fmt.Printf("Aliases: %s\n\n", strings.Join(aliases, ", "))
			}
			return
		case "ls":
			fmt.Print("mage ls:\n\n")
			fmt.Println("LS")
			fmt.Println()

			var aliases []string
			if len(aliases) > 0 {
				fmt.Printf("Aliases: %s\n\n", strings.Join(aliases, ", "))
			}
			return

		default:
			logger.Printf("Unknown target: %q\n", os.Args[1])
			os.Exit(1)
		}
	}
	if len(os.Args) < 2 {
		if err := list(); err != nil {
			logger.Println("Error:", err)
			os.Exit(1)
		}
		return
	}
	for _, target := range os.Args[1:] {
		switch strings.ToLower(target) {

		}
		switch strings.ToLower(target) {

		case "build":
			if verbose {
				logger.Println("Running target:", "Build")
			}
			wrapFn := func(ctx context.Context) error {
				return Build()
			}
			err := runTarget(wrapFn)
			handleError(logger, err)
		case "clean":
			if verbose {
				logger.Println("Running target:", "Clean")
			}
			wrapFn := func(ctx context.Context) error {
				Clean()
				return nil
			}
			err := runTarget(wrapFn)
			handleError(logger, err)
		case "install":
			if verbose {
				logger.Println("Running target:", "Install")
			}
			wrapFn := func(ctx context.Context) error {
				return Install()
			}
			err := runTarget(wrapFn)
			handleError(logger, err)
		case "installdeps":
			if verbose {
				logger.Println("Running target:", "InstallDeps")
			}
			wrapFn := func(ctx context.Context) error {
				return InstallDeps()
			}
			err := runTarget(wrapFn)
			handleError(logger, err)
		case "ls":
			if verbose {
				logger.Println("Running target:", "Ls")
			}
			wrapFn := func(ctx context.Context) error {
				return Ls()
			}
			err := runTarget(wrapFn)
			handleError(logger, err)

		default:
			// should be impossible since we check this above.
			logger.Printf("Unknown target: %q\n", os.Args[1])
			os.Exit(1)
		}
	}
}
