package main

import (
	"fmt"
	"reflect"
	"runtime"
	"runtime/debug"
)

func GetFunctionName(i interface{}) string {
	return runtime.FuncForPC(reflect.ValueOf(i).Pointer()).Name()
}

func main() {
	rt := reflect.TypeOf(fmt.Println)
	fmt.Println("@", rt, "@")
	fmt.Println(GetFunctionName(fmt.Println))
	fmt.Println(GetFunctionName(debug.Stack))
	fmt.Println("@", rt.PkgPath(), "@")
}
