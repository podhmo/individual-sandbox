package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"strconv"
)

// guess-main main.go:Run とかしてmain()を生成したい

func main() {
	nameFlag := flag.String("name", "", "name of subject") // required
	ageFlag := flag.String("age", "", "age")
	flag.Parse()

	parse := func() error {
		// handle envvar
		if os.Getenv("NAME") != "" {
			*nameFlag = os.Getenv("NAME")
		}
		if os.Getenv("AGE") != "" {
			*ageFlag = os.Getenv("AGE")
		}

		// handle required
		if *nameFlag == "" {
			return fmt.Errorf("-name is requird")
		}
		return nil
	}

	run := func() error {
		// setup components
		name := *nameFlag
		age, err := strconv.Atoi(*ageFlag)
		if err != nil {
			return fmt.Errorf("-age is not int: %w", err)
		}

		return Run(&Hello{Name: name, Age: age})
	}

	if err := parse(); err != nil {
		flag.PrintDefaults()
		log.Printf("! %+v", err)
		os.Exit(1)
	}
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Hello struct {
	Name string
	Age  int
}

func (h *Hello) Hello() {
	fmt.Println("Hello", h.Name)
}

func Run(hello *Hello) error {
	hello.Hello()
	return nil
}
