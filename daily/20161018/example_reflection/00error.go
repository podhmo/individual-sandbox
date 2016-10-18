package main

import "reflect"

// http://qiita.com/m0a/items/269acaa8ff622fe3b2c1

func main() {
	var x float64 = 3.4
	v := reflect.ValueOf(x)
	v.SetFloat(7.1) // Error: will panic.
}
