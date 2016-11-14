package main

import "fmt"

type I interface{}
type J interface{}

func ItoJ(i *I, j *J) {
	*j = 10
}

func main() {
	var i I
	var j J
	ItoJ(&i, &j)
	fmt.Println(i, j)
}
