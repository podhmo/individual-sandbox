package main

import "C"
import "fmt"
import "math"

//export Hello
func Hello() {
	fmt.Printf("Hello! The square root of 4 is: %g\n", math.Sqrt(4))
}

func main() {
	Hello()
}
