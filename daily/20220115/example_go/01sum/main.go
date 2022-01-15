package main

import "fmt"

func SumInts(xs map[string]int64) int64 {
	var s int64
	for _, x := range xs {
		s += x
	}
	return s
}

func SumFloats(xs map[string]float64) float64 {
	var s float64
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

	fmt.Println("non generics")
	fmt.Printf("%v\t%v\n", SumInts(ints), ints)
	fmt.Printf("%v\t%v\n", SumFloats(floats), floats)
}
