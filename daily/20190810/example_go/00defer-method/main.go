package main

import "fmt"

type F struct{}

func (f *F) M() {
	fmt.Println("hoi")
}

func New() *F {
	fmt.Println("hai")
	return &F{}
}

func main() {
	defer New().M()
	fmt.Println("::::::::::::::::::::::::::::::::::::::::")
}
