package main

import (
	"fmt"
	"regexp"
)

func main() {
	pattern := "o"
    rx := regexp.MustCompile(pattern)

    {
        s := "hello"
        fmt.Printf("input: %s, match %v\n", s, rx.Match([]byte(s)))
    }
    {
        s := "hmm"
        fmt.Printf("input: %s, match %v\n", s, rx.Match([]byte(s)))
    }
    {
        s := "hmm\no"
        fmt.Printf("input: %s, match %v\n", s, rx.Match([]byte(s)))
    }
}
