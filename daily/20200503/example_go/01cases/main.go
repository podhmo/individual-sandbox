package main

import (
	"fmt"
)

type IntStringUnion interface {
	Match(Cases)
}

type Cases struct {
	Int    func(Int)
	String func(String)
}

type Int int

func (i Int) Match(c Cases) { c.Int(i) }

type String string

func (i String) Match(c Cases) { c.String(i) }

func main() {
	unionArray := []IntStringUnion{
		String("1"),
		Int(2),
		String("123"),
		Int(4),
	}

	sum := 0
	for _, item := range unionArray {
		item.Match(Cases{
			Int:    func(i Int) { sum += int(i) },
			String: func(s String) { sum += len(string(s)) },
		})
	}

	fmt.Printf("%d", sum)
}
