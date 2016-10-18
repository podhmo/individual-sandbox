package main

import (
	"log"
	"reflect"
)

func Fetch10(ob interface{}) {
	reflect.ValueOf(ob).Elem().SetInt(10)
}

func main() {
	{
		x := 0
		log.Printf("before: %d\n", x)
		Fetch10(&x)
		log.Printf("after : %d\n", x)
	}
	{
		var x *int
		log.Printf("before: %d\n", x)
		Fetch10(x)
		log.Printf("after : %d\n", x)
	}
}
