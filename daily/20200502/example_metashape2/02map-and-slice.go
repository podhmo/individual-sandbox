package main

type Person struct {
	Name string
	Age int
	Parents []Person
	Memo map[string]interface{}
}
