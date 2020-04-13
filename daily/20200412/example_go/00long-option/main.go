package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
)

type wrap struct {
	Output io.Writer
}

func (w *wrap) Write(p []byte) (n int, err error) {
	s := string(p)
	// var r []string
	// for _, line := range strings.Split(s, "\n") {
	// 	if strings.HasPrefix(line, "  -") {
	// 		r = append(r, "  --"+line[3:])
	// 	} else {
	// 		r = append(r, line)
	// 	}
	// }
	// return w.Output.Write([]byte(strings.Join(r, "\n")))
	return w.Output.Write([]byte(strings.TrimPrefix(strings.ReplaceAll("\n"+s, "\n  -", "\n  --"), "\n")))
}

func main() {
	flag.CommandLine.Init("main", flag.ContinueOnError)
	flag.CommandLine.Usage = nil
	flag.CommandLine.SetOutput(&wrap{os.Stderr})

	n := flag.Int64("int64", 2, "int64 value")
	flag.Parse()
	fmt.Println(*n)
}
