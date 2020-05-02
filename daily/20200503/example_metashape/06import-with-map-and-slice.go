package main

import (
	"m/memo"
)

type Person struct {
	Name string
	Age int
	Parents []Person
	Memo map[string]memo.Memo
}
