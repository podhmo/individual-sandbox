package main

import (
	"fmt"
	"log"
	"reflect"
)

type Person struct {
	Name string
}

func Load(ob interface{}) error {
	if reflect.ValueOf(ob).IsZero() {
		return fmt.Errorf("invalid expression %+[1]v, maybe *%+[1]v?", reflect.TypeOf(ob))
	}

	rv := reflect.ValueOf(ob).Elem()
	rt := reflect.TypeOf(ob).Elem()

	var xs interface{}
	xs = XS()
	if !rt.AssignableTo(reflect.TypeOf(xs)) {
		return fmt.Errorf("invalid type: %v", rt)
	}
	rv.Set(reflect.ValueOf(xs))
	return nil
}

func XS() []Person {
	return []Person{{Name: "foo"}}
}

func main() {
	{
		var obs []Person
		if err := Load(&obs); err != nil {
			log.Fatalf("!! %+v", err)
		}
		log.Println("ok", obs)
	}

	// not panic
	{
		type Berson struct {
			Name string
		}
		var obs []Berson
		if err := Load(&obs); err != nil {
			log.Fatalf("ng %+v", err)
		}
		log.Println("ok", obs)
	}
}
