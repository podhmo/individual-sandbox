package main

import (
	"fmt"
	"io"
	"reflect"
	"runtime"
)

func Hello(
	w io.Writer,
	name string,
) error {
	_, err := fmt.Fprintln(w, "hello", name)
	return err
}

func ShowInfo(fn interface{}) {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	name := rfunc.Name()
	filename, lineno := rfunc.FileLine(rfunc.Entry())
	fmt.Println("@@", name, ":", filename, lineno)
}

func main() {
	ShowInfo(Hello)
	ShowInfo(func(
		w io.Writer,
		name string,
	) error {
		return nil
	})
}
