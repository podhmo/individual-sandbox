package main

import (
	"fmt"
	"reflect"
	"strings"
)

type MyInterface interface{}

func main() {
	{
		fmt.Println("\ninteger")
		fmt.Printf("%v == %v, %v\n", 10, 10, 10 == 10)
		fmt.Printf("%v == %v, %v\n", 10, 20, 10 == 20)
	}
	{
		i, j, k := 10, 10, 20
		fmt.Println("\nreference")
		fmt.Printf("%v == %v, %v\n", &i, &j, &i == &j)
		fmt.Printf("%v == %v, %v\n", &i, &k, &i == &k)
	}
	{
		i, j, k := "foo", "foo", "bar"
		fmt.Println("\nstring")
		fmt.Printf("%v == %v, %v\n", i, j, i == j)
		fmt.Printf("%v == %v, %v\n", i, k, i == k)
	}
	{
		xs := []string{"fo", "o"}
		i, j := "foo", strings.Join(xs, "")
		fmt.Println("\nstring2")
		fmt.Printf("%v == %v, %v\n", i, j, i == j)
	}
	{
		fmt.Println("\nnil")
		i, j, k := interface{}(nil), interface{}(nil), MyInterface(nil)
		fmt.Printf("%v == %v, %v\n", i, j, i == j)
		fmt.Printf("%v == %v, %v\n", i, k, i == k)
	}
	{
		fmt.Println("\nmap")
		i, j := map[string]int{"x": 1}, map[string]int{"x": 1}
		// fmt.Printf("%v == %v, %v\n", i, j, i == j) // error
		fmt.Printf("deep %v == %v, %v\n", i, j, reflect.DeepEqual(i, j))
	}
	{
		fmt.Println("\nslices")
		xs := [3]int{1, 2, 3}
		i, j := xs[:], xs[:]
		// fmt.Printf("%v == %v, %v\n", i, j, i == j) // error
		fmt.Printf("deep %v == %v, %v\n", i, j, reflect.DeepEqual(i, j))
	}

	type Point struct {
		x, y int
	}
	type Point2 struct {
		x int
		y int
	}

	type Point3 struct {
		x, y int
	}

	{
		fmt.Println("\nstruct")
		i, j := Point{x: 10, y: 20}, Point{x: 10, y: 20}
		fmt.Printf("%v == %v, %v\n", i, j, i == j)
		fmt.Printf("deep %v == %v, %v\n", i, j, reflect.DeepEqual(i, j))
	}

	{
		fmt.Println("\nstruct2")
		i, j := Point{x: 10, y: 20}, Point2{x: 10, y: 20}
		// fmt.Printf("%v == %v, %v\n", i, j, i == j) // error
		fmt.Printf("deep %v == %v, %v\n", i, j, reflect.DeepEqual(i, j))
	}

	{
		fmt.Println("\nstruct3")
		i, j := Point{x: 10, y: 20}, Point3{x: 10, y: 20}
		// fmt.Printf("%v == %v, %v\n", i, j, i == j) // error
		fmt.Printf("deep %v == %v, %v\n", i, j, reflect.DeepEqual(i, j))
	}
}
