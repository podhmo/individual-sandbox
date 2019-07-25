package main

import (
	"encoding/json"
	"os"
	"reflect"
)

func main() {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")

	{
		var xs []int
		encoder.Encode(reflect.MakeSlice(reflect.TypeOf(xs), 0, 0).Interface())
		encoder.Encode(xs)
	}
	{
		var xs []string
		encoder.Encode(reflect.MakeSlice(reflect.TypeOf(xs), 0, 0).Interface())
		encoder.Encode(xs)
	}
}
