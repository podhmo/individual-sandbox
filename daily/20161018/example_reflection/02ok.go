package main

import (
	"fmt"
	"reflect"
)

// http://qiita.com/m0a/items/269acaa8ff622fe3b2c1

func main() {
	var x float64 = 3.4
	p := reflect.ValueOf(&x)
	v := p.Elem()
	fmt.Println("type of v:", v.Type())
	fmt.Println("settability of v:", v.CanSet())
	v.SetFloat(7.1)
	fmt.Println(x)
}
