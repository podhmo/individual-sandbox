package main

import (
	"go/build"
	"log"
	"os"
)

// Loader :
type Loader struct {
	Mode  build.ImportMode
	Cwd   string
	Build *build.Context
}

// Default :
func Default() (*Loader, error) {
	cwd, err := os.Getwd()
	if err != nil {
		return nil, err
	}
	var mode build.ImportMode
	return &Loader{
		Mode:  mode,
		Cwd:   cwd,
		Build: &build.Default,
	}, nil
}


func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	loader, err := Default()
	if err != nil {
		return err
	}
	return nil
}
