package findcaller2

import (
	"errors"
	"fmt"
	"runtime"
)

// Recoverer ...
func Recoverer(p *error) func() {
	return func() {
		r := recover()
		if r == nil {
			return
		}
		switch r := r.(type) {
		case error:
			*p = r.(error)
		default:
			*p = errors.New(fmt.Sprintf("%v", r))
		}

		pc, filename, lineno, _ := runtime.Caller(4)
		fmt.Printf("%d:%s -- %s\n", lineno, filename, runtime.FuncForPC(pc))
	}
}
