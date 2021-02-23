package main

import (
	"fmt"
	"log"
	"reflect"
)

type Info struct {
	HasNext bool
	Value   interface{}
}

type Person struct {
	Name string
}

// Load :
// source is func() []<T> | Info<T>
func Load(source interface{}) (Info, error) {
	if source == nil {
		return Info{}, fmt.Errorf("invalid source: %+[1]v is passed, but only supports func()[]<T>", nil)
	}
	rst := reflect.TypeOf(source)
	if rst.Kind() != reflect.Func {
		return Info{}, fmt.Errorf("invalid source kind: %+[1]v is passed, but only supports func()[]<T>", rst)
	}
	if rst.NumOut() != 1 {
		return Info{}, fmt.Errorf("invalid source return type: %+[1]v is passed, but only supports func()[]<T>", rst)
	}

	retvals := reflect.ValueOf(source).Call(nil)
	val := retvals[0].Interface()
	if val, ok := val.(Info); ok {
		return val, nil
	}
	return Info{Value: val}, nil
}

func XS() []Person {
	return []Person{{Name: "foo"}}
}

func XSWithInfo() Info {
	return Info{
		HasNext: true,
		Value:   XS(),
	}
}

func main() {
	{
		info, err := Load(XS)
		if err != nil {
			log.Fatalf("ng %+v", err)
		}
		log.Printf("ok %#+v", info)
	}

	{
		info, err := Load(XSWithInfo)
		if err != nil {
			log.Fatalf("ng %+v", err)
		}
		log.Printf("ok %#+v", info)
	}
}
