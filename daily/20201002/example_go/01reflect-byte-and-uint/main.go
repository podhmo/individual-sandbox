package main

import (
	"fmt"
	"reflect"
)

type A uint8

// https://stackoverflow.com/questions/37770005/why-is-there-no-byte-kind-in-the-reflect-package
// reflect.Type上ではuint8かbyteか区別できない

func main() {
	x, y := A(1), uint8(1)
	valX, valY := reflect.ValueOf(x), reflect.ValueOf(y)
	fmt.Println("Types: x is", valX.Type(), "y is", valY.Type())
	fmt.Println("Types match:", valX.Type() == valY.Type())
	fmt.Println("Kinds: x is", valX.Kind(), "y is", valY.Kind())
	fmt.Println("Kinds match:", valX.Kind() == valY.Kind())
}

// Types: x is main.A y is uint8
// Types match: false
// Kinds: x is uint8 y is uint8
// Kinds match: true
