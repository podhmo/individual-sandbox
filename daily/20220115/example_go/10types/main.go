package main

import "fmt"

type Option [T any] struct {
	Value T
	ok bool
}

func Some[T any](v T) Option[T] {
	return Option[T]{Value: v, ok: true}
}

func main(){
	fmt.Println(Some("foo"))
}
