package main

import "fmt"

type M map[string]interface{}

func main() {
	v := M{"x": 10}
	fmt.Println(v)
}
