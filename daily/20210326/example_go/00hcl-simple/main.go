package main

import (
	"log"

	"github.com/hashicorp/hcl/v2/hclsimple"
)

func main() {
	log.SetPrefix("  ")
	log.SetFlags(0)

	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Config struct {
	LogLevel string `hcl:"log_level"`
}

func run() error {
	var config Config
	filename := "config.hcl"
	log.Printf("Load file %q", filename)
	err := hclsimple.DecodeFile(filename, nil, &config)
	if err != nil {
		return err
	}
	log.Printf("Configuration is %#v", config)
	return nil
}
