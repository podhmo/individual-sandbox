package main

import "fmt"

func SumIntsOrFloats[K comparable, V int64 | float64](xs map[K]V) V {
	var s V
	for _, x := range xs {
		s += x
	}
	return s
}

func main() {
	ints := map[string]int64{
		"first":  34,
		"second": 12,
	}
	floats := map[string]float64{
		"first":  35.98,
		"second": 26.99,
	}

	fmt.Println("generics")
	fmt.Printf("%v\t%v\n", SumIntsOrFloats[string, int64](ints), ints)
	fmt.Printf("%v\t%v\n", SumIntsOrFloats[string, float64](floats), floats)
}
