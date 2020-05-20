package main

import (
	"flag"
	"fmt"
	"os"
	"log"
	"m/internal/hello"
)

// this file is generated by egoist.generators.clikit

// Option ...
type Option struct {
	Name string // for `-name`
	Age int // for `-age`
	Who string // for `-who`
	Args []string // cmd.Args
}


func main() {
	opt := &Option{}
	cmd := flag.NewFlagSet("hello", flag.ContinueOnError)
	cmd.Usage = func() {
		fmt.Fprintln(cmd.Output(), `hello - hello message`)
		fmt.Fprintln(cmd.Output(), "")
		fmt.Fprintln(cmd.Output(), "Usage:")
		cmd.PrintDefaults()
	}
	cmd.StringVar(&opt.Name, "name", "", "the name of target person")
	cmd.IntVar(&opt.Age, "age", 20, "age of subject")
	cmd.StringVar(&opt.Who, "who", "foo", "name of subject")

	if err := cmd.Parse(os.Args[1:]); err != nil {
		if err != flag.ErrHelp {
			cmd.Usage()
		}
		os.Exit(1)
	}
	opt.Args = cmd.Args()
	if err := run(opt); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(opt *Option) error {
	hello.Hello(opt.Name, opt.Age, opt.Who)
	return nil
}