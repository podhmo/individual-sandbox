package main

import (
	"flag"
	"fmt"
	"log"
	"os"
)

// guess-main main.go:Run とかしてmain()を生成したい

func main() {
	nameFlag := flag.String("name", "", "name of subject")
	flag.Parse()

	parse := func() error {
		// handle envvar
		if os.Getenv("NAME") != "" {
			*nameFlag = os.Getenv("NAME")
		}

		// handle required
		if *nameFlag == "" {
			return fmt.Errorf("-name is requird")
		}
		return nil
	}

	run := func() error {
		// handle components
		return Run(&Hello{Name: *nameFlag})
	}

	if err := parse(); err != nil {
		flag.PrintDefaults()
		os.Exit(1)
	}
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Hello struct {
	Name string
}

func (h *Hello) Hello() {
	fmt.Println("Hello", h.Name)
}

func Run(hello *Hello) error {
	hello.Hello()
	return nil
}
