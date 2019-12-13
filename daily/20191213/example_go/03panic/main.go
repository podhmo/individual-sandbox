package main

import (
	"fmt"
	"m/findcaller"
	"regexp"
	"runtime"
	"strings"
)

func main() {
	foo()
}

func foo() {
	bar()
}

type person struct {
	Name string
}

func bar() {
	defer func() {
		r := recover()
		if r == nil {
			return
		}

		trace := make([]byte, 1024*10)
		c := runtime.Stack(trace, true)

		line, err := findcaller.FindCallerFromStackTrace(trace[:c],
			findcaller.WithDepth(2),
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
	}()
	var p *person
	fmt.Println(p.Name) // nil panic
}
