package main

import (
	"fmt"
	"reflect"
)

func islice(xs interface{}) []interface{} {
	rxs := reflect.ValueOf(xs)
	if rxs.Type().Kind() != reflect.Slice {
		panic(fmt.Sprintf("unexpected type: %T", xs))
	}

	ys := make([]interface{}, rxs.Len())
	for i := 0; i < rxs.Len(); i++ {
		ys[i] = rxs.Index(i).Interface()
	}
	return ys
}

func main() {
	{
		xs := []string{"foo", "bar", "boo"}
		fmt.Printf("%[1]T:%+[1]v	%[2]T:%+[2]v\n", islice(xs), xs)
	}
	{
		xs := []int{1, 2, 3}
		fmt.Printf("%[1]T:%+[1]v	%[2]T:%+[2]v\n", islice(xs), xs)
	}
}
