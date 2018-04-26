package main

import (
	"errors"
	"fmt"
)

type person struct {
	name string
	age  int
}

var (
	errNotFound = errors.New("404")
	pool        = map[int]*person{
		1: &person{name: "foo", age: 10},
	}
)

func findPerson(id int) (*person, error) {
	person, ok := pool[id]
	if !ok {
		return person, errNotFound
	}
	return person, nil
}

func main() {
	{
		p, err := findPerson(1)
		fmt.Printf("%#+v, (error=%v)\n", p, err)
	}
	{
		p, err := findPerson(2)
		fmt.Printf("%#+v, (error=%v)\n", p, err)
	}
}
