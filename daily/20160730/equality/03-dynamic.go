package main

import (
	"fmt"
)

type Point struct {
	x, y int
}
type Point2 struct {
	x, y int
}

type MyInterface interface {
}

func main(){
    i, j, k := Point{x: 10, y: 20}, Point{x: 10, y: 20}, Point2{x: 10, y: 20}
    fmt.Printf("%v == %v, %v\n", i, j, i == j)
    // fmt.Printf("%v == %v, %v\n", i, k, i == k) // compile error
    fmt.Printf("%v == %v, %v\n", i, k, i == Point(k)) // i == k true
    fmt.Printf("%v == %v, %v\n", i, k, MyInterface(i) == MyInterface(k)) // i == k false
    fmt.Printf("%v == %v, %v\n", i, k, interface {}(i) == interface {}(k)) // i == k false
}
