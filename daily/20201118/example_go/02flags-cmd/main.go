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

	cmd := flag.NewFlagSet("app", flag.ContinueOnError)
	cmd.StringVar(&option.name, "name", "", "-")
	cmd.IntVar(&option.age, "age", 0, "-")

	cmd.Usage = func() {
		cmd.PrintDefaults()
		fmt.Fprintf(cmd.Output(), `
Or, you can pass a value via envvar, such as "NAME = foo AGE = 20 %s".
`, cmd.Name())
	}

	run := func(args []string) (rawErr error) {
		defer func() {
			if err := recover(); err != nil {
				rawErr = fmt.Errorf("%+v", err)
			}
		}()
		if err := cmd.Parse(args); err != nil {
			return err
		}
		cmd.VisitAll(func(f *flag.Flag) {
			envname := strings.ToUpper(f.Name)
			if v := os.Getenv(envname); v != "" {
				if err := cmd.Set(f.Name, v); err != nil {
					panic(fmt.Sprintf("on envvar %s=%v, %v", envname, v, err))
				}
			}
		})
		return Run(option.name, option.age)
	}

	log.SetFlags(0)
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("\x1b[0;33m!! %+v\x1b[0m", err)
	}
}

func Run(name string, age int) error {
	fmt.Printf("%s(%d): hello\n", name, age)
	return nil
}
