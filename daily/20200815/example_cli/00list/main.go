package main

import (
	"log"

	"github.com/cli/cli/command"
	"github.com/cli/cli/pkg/cmd/factory"
	"github.com/k0kubun/pp"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	cmdFactory := factory.New(command.Version)
	cfg, err := cmdFactory.Config()
	if err != nil {
		return err
	}
	pp.Println(cfg)
}
