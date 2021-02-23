package main

import (
	"fmt"
	"log"
	"reflect"
)

type Info struct {
	HasNext bool
}

type Person struct {
	Name string
}

// Load :
// source is func()[]T | func()([]T,error) |  | func()([]T,Info,error)
func Load(source interface{}) (interface{}, Info, error) {
	if source == nil {
		return nil, Info{}, fmt.Errorf("invalid source: %+[1]v is passed, but only supports func()[]<T>", nil)
	}
	rst := reflect.TypeOf(source)
	if rst.Kind() != reflect.Func {
		return nil, Info{}, fmt.Errorf("invalid source kind: %+[1]v is passed, but only supports func()[]<T>", rst)
	}

	switch rst.NumOut() {
	case 1:
		// func()[]T
		retvals := reflect.ValueOf(source).Call(nil)
		val := retvals[0].Interface()
		return val, Info{}, nil
	case 2:
		// func()([]T, error)
		if !rst.Out(1).Implements(rerrorType) {
			return nil, Info{}, fmt.Errorf("invalid source return type: %+[1]v is passed, but only supports func()([]<T>, error)", rst)
		}
		retvals := reflect.ValueOf(source).Call(nil)
		val := retvals[0].Interface()
		err := retvals[1].Interface()
		if err == nil {
			return val, Info{}, nil
		}
		return val, Info{}, nil
	case 3:
		// func()([]T, Info, error)
		if !rst.Out(2).Implements(rerrorType) {
			return nil, Info{}, fmt.Errorf("invalid source return type: %+[1]v is passed, but only supports func()([]<T>, Info, error)", rst)
		}
		retvals := reflect.ValueOf(source).Call(nil)
		val := retvals[0].Interface()
		info, t := retvals[1].Interface().(Info)
		if !t {
			return nil, Info{}, fmt.Errorf("invalid source info type: %+[1]v is passed, but only supports func()([]<T>, Info, error)", rst)
		}
		err := retvals[2].Interface()
		if err == nil {
			return val, info, nil
		}
		return val, info, err.(error)
	default:
		return nil, Info{}, fmt.Errorf("unexpected source return type: %+[1]v is passed, but only supports func()[]<T>", rst)
	}
}

var rerrorType reflect.Type

func init() {
	rerrorType = reflect.TypeOf(func() error { return nil }).Out(0)
}

func XS() []Person {
	return []Person{{Name: "foo"}}
}
func XSWithInfo() ([]Person, Info, error) {
	return []Person{{Name: "foo"}}, Info{HasNext: true}, nil
}
func XSWithError() ([]Person, error) {
	return []Person{{Name: "foo"}}, nil
}

func main() {
	{
		xs, info, err := Load(XS)
		if err != nil {
			log.Fatalf("ng %+v", err)
		}
		log.Printf("ok %#+v, %#+v", xs, info)
	}
	{
		xs, info, err := Load(XSWithInfo)
		if err != nil {
			log.Fatalf("ng %+v", err)
		}
		log.Printf("ok %#+v, %#+v", xs, info)
	}
	{
		xs, info, err := Load(XSWithError)
		if err != nil {
			log.Fatalf("ng %+v", err)
		}
		log.Printf("ok %#+v, %#+v", xs, info)
	}
}
