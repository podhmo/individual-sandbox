package main

import (
	"fmt"
	"io"

	"github.com/pkg/errors"
)

type wrap struct {
	Msg string
	Err error
}

func (w *wrap) Error() string {
	return w.Msg + " " + w.Err.Error()
}

func (w *wrap) Format(s fmt.State, verb rune) {
	switch verb {
	case 'v':
		if s.Flag('+') {
			fmt.Fprintf(s, "%s ---- %+v", w.Msg, w.Err)
			return
		}
		fallthrough
	case 's':
		io.WriteString(s, w.Error())
	case 'q':
		fmt.Fprintf(s, "%q", w.Error())
	}
}

func main() {
	run()
}
func run() {
	fmt.Printf("!! %+v", f())
	fmt.Printf("!! %+v", &wrap{Msg: "hello", Err: f()})
}
func f() error {
	return errors.Wrap(g(), "F")
}
func g() error {
	return fmt.Errorf("hmm")
}
