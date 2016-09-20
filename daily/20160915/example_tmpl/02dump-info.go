package main

import (
	"fmt"
	"os"
	"runtime"
)

func main() {
	fmt.Printf("go version: %q\n", runtime.Version())
	pwd, _ := os.Getwd()
	fmt.Printf("cwd: %q\n", pwd)
}
