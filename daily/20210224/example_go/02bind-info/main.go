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

func Load(ob interface{}, load func(*interface{})) error {
	if reflect.ValueOf(ob).IsZero() {
		return fmt.Errorf("invalid expression %+[1]v, maybe *%+[1]v?", reflect.TypeOf(ob))
	}

	rv := reflect.ValueOf(ob).Elem()
	rt := reflect.TypeOf(ob).Elem()

	var xs interface{}
	load(&xs)
	if !rt.AssignableTo(reflect.TypeOf(xs)) {
		return fmt.Errorf("invalid type: %v", rt)
	}
	rv.Set(reflect.ValueOf(xs))
	return nil
}

func XS(ob *interface{}) {
	*ob = []Person{{Name: "foo"}}
}

func XSWithInfo(ob *interface{}) {
	info := Info{HasNext: true}
	XS(&info.Value)
	*ob = info
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
