package main

import (
	"fmt"
	"reflect"
)

type Foo = string
type Bar string

func main() {
	{
		v := "xxx"
		rt := reflect.TypeOf(v)
		fmt.Printf("%[1]T	%+[1]v	%[2]v\n", rt, rt.Kind())
	}
	{
		v := Foo("xxx")
		rt := reflect.TypeOf(v)
		fmt.Printf("%[1]T	%+[1]v	%[2]v\n", rt, rt.Kind())
	}
	{
		v := Bar("xxx")
		rt := reflect.TypeOf(v)
		fmt.Printf("%[1]T	%+[1]v	%[2]v\n", rt, rt.Kind())
	}
}
