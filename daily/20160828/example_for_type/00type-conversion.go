package main

// type conversion(casting)

import (
	"fmt"
)

type myint int

func main() {
	{
		target := 10
		fmt.Printf("%[1]T, %#+[1]v\n", target)
	}
	{
		target := 10
		fmt.Printf("%[1]T, %#+[1]v\n", myint(target))
	}
}
