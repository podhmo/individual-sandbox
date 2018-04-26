package main

import (
	"errors"
	"fmt"
	"reflect"
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

func findPerson(id int, v interface{}) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Ptr || rv.IsNil() {
		return fmt.Errorf("invalid %v", reflect.TypeOf(v))
	}
	ob, ok := pool[id]
	if !ok {
		return errNotFound
	}
	rob := reflect.ValueOf(ob)
	if rob.Kind() == reflect.Ptr {
		rob = rob.Elem()
	}
	rv.Elem().Set(rob)
	return nil
}

func main() {
	{
		var p person
		err := findPerson(1, &p)
		fmt.Printf("%#+v, (error=%v)\n", p, err)
	}
	{
		var p person
		err := findPerson(2, &p)
		fmt.Printf("%#+v, (error=%v)\n", p, err)
	}
}
