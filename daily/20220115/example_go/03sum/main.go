package main

import "fmt"

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
	ints := map[string]int64{
		"first":  34,
		"second": 12,
	}
	floats := map[string]float64{
		"first":  35.98,
		"second": 26.99,
	}

	fmt.Println("generics")
	fmt.Printf("%v\t%v\n", SumNumbers(ints), ints)
	fmt.Printf("%v\t%v\n", SumNumbers(floats), floats)
}
