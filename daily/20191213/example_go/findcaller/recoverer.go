package findcaller

import (
	"fmt"
	"os"
	"regexp"
	"runtime"
	"strings"
)

// Recoverer ...
func Recoverer() func() {
	return func() {
		r := recover()
		if r == nil {
			return
		}

		fmt.Fprintf(os.Stderr, "--%#v--\n", r)

		trace := make([]byte, 1024*10)
		c := runtime.Stack(trace, false)

		line, err := FindCallerFromStackTrace(trace[:c],
			WithDepth(2),
		)
		if err != nil {
			fmt.Println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
			fmt.Println(err)
			fmt.Println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
			return
		}

		rx := regexp.MustCompile(`\.go:(\d+)`)
		filename := strings.SplitN(line, ":", 2)[0]
		lineno := string(rx.FindSubmatch([]byte(line))[1])
		fmt.Printf("%s:%s\n", lineno, filename)
	}
}
