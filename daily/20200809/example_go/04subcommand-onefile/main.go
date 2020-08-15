package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"strings"
)

type App struct {
	Name string
	Cmd  *flag.FlagSet
	Args *[]string
	Run  func() error
}

func NewHelloApp() *App {
	var opt struct {
		Name string   // for `-name`
		Args []string // cmd.Args
	}

	cmd := flag.NewFlagSet("hello message", flag.ExitOnError)
	cmd.Usage = func() {
		fmt.Fprintln(cmd.Output(), `hello - hello message`)
		fmt.Fprintln(cmd.Output(), "")
		fmt.Fprintln(cmd.Output(), "Usage:")
		cmd.PrintDefaults()
	}
	cmd.StringVar(&opt.Name, "name", "", "name of person") // todo: required

	return &App{
		Name: "hello",
		Cmd:  cmd,
		Args: &opt.Args,
		Run: func() error {
			fmt.Println("Hello", opt.Name, opt.Args)
			return nil
		},
	}
}

func NewByebyeApp() *App {
	var opt struct {
		Name string   // for `-name`
		Args []string // cmd.Args
	}

	cmd := flag.NewFlagSet("byebye message", flag.ExitOnError)
	cmd.Usage = func() {
		fmt.Fprintln(cmd.Output(), `byebye - byebye message`)
		fmt.Fprintln(cmd.Output(), "")
		fmt.Fprintln(cmd.Output(), "Usage:")
		cmd.PrintDefaults()
	}
	cmd.StringVar(&opt.Name, "name", "", "name of person") // todo: required

	return &App{
		Name: "byebye",
		Cmd:  cmd,
		Args: &opt.Args,
		Run: func() error {
			fmt.Println("Byebye", opt.Name, opt.Args)
			return nil
		},
	}
}

func main() {
	candidates := []*App{
		NewHelloApp(),
		NewByebyeApp(),
	}

	cmd := flag.NewFlagSet("app", flag.ContinueOnError)
	cmd.Usage = func() {
		fmt.Fprintln(cmd.Output(), `app - main app`)
		fmt.Fprintln(cmd.Output(), "")
		fmt.Fprintln(cmd.Output(), "Usage:")
		fmt.Fprintf(cmd.Output(), "  app [%s]\n", strings.Join([]string{"hello", "byebye"}, "|"))
		fmt.Fprintln(cmd.Output(), "")
		fmt.Fprintln(cmd.Output(), "Subcommands:")
		for _, app := range candidates {
			fmt.Fprintln(cmd.Output(), "  ", app.Name, "-", app.Cmd.Name())
		}
		cmd.PrintDefaults()
	}

	if err := cmd.Parse(os.Args[1:]); err != nil {
		if err != flag.ErrHelp {
			cmd.Usage()
		}
		os.Exit(1)
	}
	if cmd.NArg() <= 0 {
		cmd.Usage()
		os.Exit(2)
	}

	args := cmd.Args()
	var app *App
	for i := range candidates {
		if candidates[i].Name == args[0] {
			app = candidates[i]
			break
		}
	}
	if app == nil {
		cmd.Usage()
		os.Exit(3)
	}
	if err := app.Cmd.Parse(args[1:]); err != nil {
		if err != flag.ErrHelp {
			app.Cmd.Usage()
		}
		os.Exit(4)
	}
	*app.Args = app.Cmd.Args()
	if err := app.Run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}
