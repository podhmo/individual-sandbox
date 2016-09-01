package cmd

import (
	"github.com/mitchellh/cli"
)

type CommandsBuilder interface {
	AddCommand(name string, factory cli.CommandFactory)
	Build() map[string]cli.CommandFactory
}

type rootCmd struct {
	Factories map[string]cli.CommandFactory
}

func (rc *rootCmd) AddCommand(name string, factory cli.CommandFactory) {
	rc.Factories[name] = factory
}
func (rc *rootCmd) Build() map[string]cli.CommandFactory {
	return rc.Factories
}

var RootCmd CommandsBuilder = &rootCmd{Factories: make(map[string]cli.CommandFactory)}
