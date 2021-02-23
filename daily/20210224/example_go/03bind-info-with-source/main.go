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
// ob is []<T> | Info<T>
// source is func() []<T> | Info<T>
func Load(ob interface{}, source interface{}) error {
	if reflect.ValueOf(ob).IsZero() {
		return fmt.Errorf("invalid expression %+[1]v, maybe *%+[1]v?", reflect.TypeOf(ob))
	}

	rst := reflect.TypeOf(source)
	if rst.Kind() != reflect.Func {
		return fmt.Errorf("invalid source kind: %+[1]v is passed, only suppor func()[]<T>", rst)
	}
	if rst.NumOut() != 1 {
		return fmt.Errorf("invalid source return type: %+[1]v is passed, but only suppor func()[]<T>", rst)
	}

	rt := reflect.TypeOf(ob).Elem()
	if !rt.AssignableTo(rst.Out(0)) {
		return fmt.Errorf("invalid type: got %[1]v, but %[2]v is expected", rt, rst.Out(0))
	}

	retvals := reflect.ValueOf(source).Call(nil)

	rv := reflect.ValueOf(ob)
	rv.Elem().Set(retvals[0])
	return nil
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
		var obs []Person
		if err := Load(&obs, XS); err != nil {
			log.Fatalf("ng %+v", err)
		}
		log.Printf("ok %#+v", obs)
	}

	{
		var info Info
		if err := Load(&info, XSWithInfo); err != nil {
			log.Fatalf("ng %+v", err)
		}
		log.Printf("ok %#+v", info)
	}
}
