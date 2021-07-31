package main

import "fmt"

type DB struct{}
type Redis struct{}
type Queue struct{}

type Foo interface {
	DB() (*DB, error)
	Queue() (*Queue, error)
}

type Bar interface {
	DB() (*DB, error)
	Redis() (*Redis, error)
}

type Resolver interface {
	Foo
	Bar
}

func main() {
	var r Resolver
	fmt.Println(r)
}
