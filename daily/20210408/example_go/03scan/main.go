package main

import (
	"fmt"
	"log"

	"github.com/podhmo/validator/tagscan"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Config struct {
	Color     string `validate:"@regexp=^(#[0-9a-fA-F]{3}|)$"`
	SubConfig *ColorConfig
}

type ColorConfig struct {
	Color string `validate:"@regexp=^(#[0-9a-fA-F]{3}|)$"`
}

func run() error {
	c := tagscan.NewConfigDefault()
	c.DetectStruct = true

	xs, err := c.Scanner().ScanAll(Config{})
	if err != nil {
		return err
	}
	for _, x := range xs {
		fmt.Println(x.Type)
		fmt.Println(x.Describe())
	}
	return nil
}
