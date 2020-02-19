package main

import (
	"encoding/json"
	"log"
	"m/conf"
	"os"

	"github.com/k0kubun/pp"
	"github.com/spf13/pflag"
)

// JSONLoadFile ...
func JSONLoadFile(filename string, c interface{}) error {
	f, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer f.Close()
	decoder := json.NewDecoder(f)
	return decoder.Decode(c)
}

var (
	// Cmd ...
	Cmd    = pflag.CommandLine
	config = Cmd.String("config", "", "config")
)

func main() {
	Cmd.Parse(os.Args[1:])
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	config := *config

	var c conf.Config
	if err := JSONLoadFile(config, &c); err != nil {
		return err
	}

	pp.ColoringEnabled = false
	pp.Println(c)
	return nil
}
