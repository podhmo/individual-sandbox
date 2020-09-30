package main

import (
	"context"
	"fmt"
	"m/shape"
	"reflect"

	"github.com/k0kubun/pp"
)

func Dump(s shape.Shape) {
	fmt.Printf("! %+v\n", s)
}

type Todo struct {
	Title string
}

func ListTodo(ctx context.Context) ([]*Todo, error) {
	return nil, nil
}

func main() {
	n := 100

	Dump(shape.Extract(n))
	Dump(shape.Extract(&n))

	Dump(shape.Extract(Todo{}))
	Dump(shape.Extract(&Todo{}))

	Dump(shape.Extract([]Todo{}))
	Dump(shape.Extract([]*Todo{}))
	Dump(shape.Extract(map[string]Todo{}))
	Dump(shape.Extract(map[Todo]map[string]int{}))

	Dump(shape.Extract(ListTodo))
	Dump(shape.Extract(func(fmt string, args ...interface{}) (int, error) { return 0, nil }))
	Dump(shape.Extract(reflect.ValueOf))
	Dump(shape.Extract(pp.Println))

	Dump(shape.Extract(shape.Extract))
}
