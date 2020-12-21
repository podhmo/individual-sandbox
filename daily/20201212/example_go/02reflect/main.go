package main

import (
	"fmt"
	"reflect"
)

type Foo string
type Bar string
type Alias = string

func Hello(string) string {
	return ""
}
func Byebye(string) string {
	return ""
}

func main() {
	fmt.Println(reflect.TypeOf(func() string { return "" }()))
	fmt.Println("----------------------------------------")
	fmt.Println(reflect.TypeOf(func() Foo { return Foo("") }()))
	fmt.Println(reflect.TypeOf(func() Bar { return Bar("") }()))
	fmt.Println("----------------------------------------")
	fmt.Println(reflect.TypeOf(func() Alias { return Alias("") }()))
	fmt.Println("========================================")
	fmt.Println(reflect.TypeOf(func() interface{} { return Hello }()))
	fmt.Println(reflect.TypeOf(func() interface{} { return Byebye }()))
}

// string
// ----------------------------------------
// main.Foo
// main.Bar
// ----------------------------------------
// string
// ========================================
// func(string) string
// func(string) string
