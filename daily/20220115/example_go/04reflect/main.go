package main

import "fmt"
import "reflect"

type Number interface {
	int64 | float64
}

func SumNumbers[K comparable, V Number](xs map[K]V) V {
	var s V
	for _, x := range xs {
		s += x
	}
	return s
}

func main() {
	// ERROR: cannot use generic function SumNumbers without instatiation.
	// fmt.Println(reflect.TypeOf(SumNumbers))

	fmt.Println(reflect.TypeOf(SumNumbers[string,int64])) // func(map[string,int64) int64
}
