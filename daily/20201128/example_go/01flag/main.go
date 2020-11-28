package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

// type Value interface {
// 	String() string
// 	Set(string) error
// }

type FileContenOrLiteralValue string

func (v *FileContenOrLiteralValue) String() string {
	return string(*v)
}

func (v *FileContenOrLiteralValue) Set(s string) error {
	if !strings.HasPrefix(s, "@") {
		*v = FileContenOrLiteralValue(s)
		return nil
	}

	b, err := ioutil.ReadFile(strings.TrimPrefix(s, "@"))
	if err != nil {
		return err
	}
	*v = FileContenOrLiteralValue(string(b))
	return nil
}

func main() {
	var options struct {
		Target string
	}
	fs := flag.NewFlagSet("app", flag.ExitOnError)
	fs.Var((*FileContenOrLiteralValue)(&options.Target), "target", "literal or @<targetname>")

	fs.Parse(os.Args[1:])
	fmt.Println(options.Target)
}
