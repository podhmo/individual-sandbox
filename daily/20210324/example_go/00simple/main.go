package main

import (
	"fmt"
	"log"

	"github.com/podhmo/validator/tag"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Config struct {
	Color     string              `validate:"@regexp=#[0-9a-fA-F]{3}"`
	Colors    []string            `validate:"@@regexp=#[0-9a-fA-F]{3} @min-length=2"`
	ColorData map[string][]string `validate:"@@@regexp=#[0-9a-fA-F]{3} @@min-length=2 @max-length=3 @min-length=1"`
}

func run() error {
	s := tag.NewScannerDefault()
	s.Tag = "validate"
	kludge, err := s.Scan(Config{})
	if err != nil {
		return err
	}

	fmt.Println(kludge.Describe())
	fmt.Println("----------------------------------------")
	for _, code := range kludge.Code {
		fmt.Println(code.Addr, code.Op, "-", code.Args)
		if code.Op == tag.OpDeField {
			fmt.Println("")
		}
	}
	return nil
}
