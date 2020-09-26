package main

import (
	"fmt"
)

func NilToEmpty(v interface{}) interface{} {
	if v == nil {
		return []bool{}
	}

	// val := reflect.ValueOf(v)
	// if val.Kind() == reflect.Slice && val.IsNil() {
	// 	v = reflect.MakeSlice(val.Type(), 0, 0).Interface()
	// }
	return v
}

func main() {
	{
		fmt.Println(nil, NilToEmpty(nil))
	}
	{
		var v interface{}
		v = nil
		fmt.Println(v, NilToEmpty(v))
	}
	{
		var v []int
		fmt.Println(v, NilToEmpty(v))
	}
	{
		v := func() []bool {
			return nil
		}()
		fmt.Println(func() []bool {
			return nil
		}())
		fmt.Println(v, NilToEmpty(v))
	}
}
