package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/google/logger"
)

var verbose = flag.Bool("verbose", false, "print info level logs to stdout")

func main() {
	flag.Parse()

	defer logger.Init("LoggerExample", *verbose, true, os.Stdout).Close()

	logger.Info("I'm about to do something!")
	if err := doSomething(); err != nil {
		logger.Errorf("Error running doSomething: %v", err)
	}
}

func doSomething() error {
	return fmt.Errorf("hmm")
}
