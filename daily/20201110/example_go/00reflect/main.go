package main

import (
	"fmt"
	"reflect"
)

type IFoo interface {
	Foo() string
}

type Foo struct {
	Name string
}

func (ob *Foo) Foo() string {
	return "Foo"
}

type WFoo struct {
	Name string
	*Foo
}
type WFoo2 struct {
	Name string
	Foo
}
type WFoo3 struct {
	Name string
	WFoo
}
type WFoo4 struct {
	Name string
	*WFoo
}

func DeepImplements(t reflect.Type, u reflect.Type) bool {
	if t.Implements(u) {
		return true
	}
	if t.Kind() == reflect.Ptr {
		return DeepImplements(t.Elem(), u)
	}
	if t.NumField() <= 0 {
		return false
	}
	for i := 0; i < t.NumField(); i++ {
		f := t.Field(i)
		if !f.Anonymous {
			continue
		}
		if DeepImplements(f.Type, u) {
			return true
		}
	}
	return false
}

func main() {
	riface := reflect.TypeOf((*IFoo)(nil)).Elem()
	fmt.Println(riface)

	{
		ob := Foo{}
		rt := reflect.TypeOf(ob)
		fmt.Println(rt, "implments", rt.Implements(riface), "DeepImplements", DeepImplements(rt, riface))
	}

	{
		ob := &Foo{}
		rt := reflect.TypeOf(ob)
		fmt.Println(rt, "implments", rt.Implements(riface), "DeepImplements", DeepImplements(rt, riface))
	}

	{
		ob := &WFoo{}
		rt := reflect.TypeOf(ob)
		fmt.Println(rt, "implments", rt.Implements(riface), "DeepImplements", DeepImplements(rt, riface))
	}
	{
		ob := &WFoo2{}
		rt := reflect.TypeOf(ob)
		fmt.Println(rt, "implments", rt.Implements(riface), "DeepImplements", DeepImplements(rt, riface))
	}
	{
		ob := &WFoo3{}
		rt := reflect.TypeOf(ob)
		fmt.Println(rt, "implments", rt.Implements(riface), "DeepImplements", DeepImplements(rt, riface))
	}
	{
		ob := &WFoo4{}
		rt := reflect.TypeOf(ob)
		fmt.Println(rt, "implments", rt.Implements(riface), "DeepImplements", DeepImplements(rt, riface))
	}
}
