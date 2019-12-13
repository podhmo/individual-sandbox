package findcaller2

import (
	"fmt"
	"os"
	"runtime"
)

// Recoverer ...
func Recoverer() func() {
	return func() {
		r := recover()
		if r == nil {
			return
		}

		fmt.Fprintf(os.Stderr, "--%#v--\n", r)
		pc, filename, lineno, _ := runtime.Caller(4)
		fmt.Printf("%d:%s -- %s\n", lineno, filename, runtime.FuncForPC(pc))
	}
}
