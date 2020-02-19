package main

import (
	"encoding/json"
	"fmt"
	"log"
	"m/conf"
	"os"

	"github.com/imdario/mergo"
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

// LoadConfig ...
func LoadConfig(filename string) (*conf.Config, error) {
	var c conf.Config
	if err := JSONLoadFile(filename, &c); err != nil {
		return nil, err
	}

	overwritefile := os.Getenv("OVERWRITE_CONFIG")
	if overwritefile == "" {
		return &c, nil
	}

	fmt.Fprintf(os.Stderr, "***** OVERWRITE CONFIG by %q *****\n", overwritefile)

	var c2 conf.Config
	if err := JSONLoadFile(overwritefile, &c2); err != nil {
		return &c, err
	}
	if err := mergo.Merge(&c2, &c); err != nil {
		return &c, err
	}
	return &c2, nil
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

	c, err := LoadConfig(config)
	if err != nil {
		return err
	}

	pp.ColoringEnabled = false
	pp.Println(c)
	return nil
}
