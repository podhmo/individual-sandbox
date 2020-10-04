package main

import (
	"fmt"
	"log"
	"reflect"
	"runtime"
)

// F : this is F
func F() {
}

func main() {
	if err := run(); err != nil {
		log.Printf("!! %+v", err)
	}
}
func run() error {
	fn := F
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())

	fmt.Println("entry is ", rfunc.Entry())
	fmt.Println("name is ", rfunc.Name())
	fmt.Printf("position is ")
	fmt.Println(rfunc.FileLine(rfunc.Entry()))
	return nil
}
