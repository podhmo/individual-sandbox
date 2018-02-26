package main

import "fmt"

type s struct {
}

func (s *s) Hello() string {
	return "Hello"
}

type ss struct {
	*s
}

func main() {
	var s *s
	fmt.Println(s.Hello())
	var ss *ss
	fmt.Println(ss.Hello())
}
