package main

import (
	"fmt"
	"reflect"

	"./internal"
	"github.com/k0kubun/pp"
)

func main() {
	pp.Println(internal.Default)

	fmt.Println(reflect.ValueOf(internal.Default).FieldByName("y").Int())
}
