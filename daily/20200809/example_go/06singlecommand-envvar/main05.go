package main

import (
	"flag"
	"fmt"
	"log"
	"os"
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

func main() {
	app := NewHelloApp()
	if err := app.Cmd.Parse(os.Args[1:]); err != nil {
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
