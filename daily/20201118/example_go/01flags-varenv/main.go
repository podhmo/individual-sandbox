package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	var option struct {
		name string
		age  int
	}
	flag.StringVar(&option.name, "name", "", "-")
	flag.IntVar(&option.age, "age", 0, "-")

	run := func() error {
		flag.Parse()
		flag.VisitAll(func(f *flag.Flag) {
			envname := strings.ToUpper(f.Name)
			if envval := os.Getenv(envname); envval != "" {
				flag.Set(f.Name, envval)
			}
		})
		return Run(option.name, option.age)
	}
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Run(name string, age int) error {
	fmt.Printf("%s(%d): hello\n", name, age)
	return nil
}
