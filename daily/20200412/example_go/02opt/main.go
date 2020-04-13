package main

import (
	"flag"
	"log"
	"os"

	"github.com/k0kubun/pp"
)

type Option struct {
	Int64 int64
}

func main() {
	opt := &Option{}
	cmd := flag.NewFlagSet("app", flag.ContinueOnError)
	cmd.Int64Var(&opt.Int64, "int64", 2, "int64 value")

	if err := cmd.Parse(os.Args[1:]); err != nil {
		cmd.Usage()
		os.Exit(1)
	}
	if err := run(opt); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(opt *Option) error {
	pp.Println(opt)
	return nil
}
