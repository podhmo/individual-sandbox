package main

import (
	"fmt"
	"reflect"
)

type MyInt int

func main() {
	{
		var z int
		rt := reflect.TypeOf(z)
		fmt.Printf("%q	%q\n", rt.String(), rt.PkgPath())
		// "int"	""
	}
	{
		var z MyInt
		rt := reflect.TypeOf(z)
		fmt.Printf("%q	%q\n", rt.String(), rt.PkgPath())
		// "main.MyInt"	"main"
	}
}
