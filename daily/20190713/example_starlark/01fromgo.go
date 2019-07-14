package main

import (
	"fmt"
	"log"

	"go.starlark.net/starlark"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	// Execute Starlark program in a file.
	thread := &starlark.Thread{Name: "my thread"}
	globals, err := starlark.ExecFile(thread, "fibonacci.star", nil, nil)
	if err != nil {
		return err
	}

	// Retrieve a module global.
	fibonacci := globals["fibonacci"]

	// Call Starlark function from Go.
	v, err := starlark.Call(thread, fibonacci, starlark.Tuple{starlark.MakeInt(10)}, nil)
	if err != nil {
		return err

	}
	fmt.Printf("fibonacci(10) = %v\n", v) // fibonacci(10) = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
	return nil
}
