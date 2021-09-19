package main

import "fmt"

func main() {
	fmt.Println(Do())
}

func Do() (interface{}, error) {
	return RunFoo()
}

func RunFoo() (*Foo, error) {
	return &Foo{}, nil
}

type Foo struct{}
