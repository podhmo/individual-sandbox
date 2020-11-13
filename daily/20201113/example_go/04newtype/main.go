package main

import "fmt"

type Person struct {
	Name string
}

func (p *Person) Hello() string {
	return p.Name + ": Hello"
}

func main() {
	p := Person{Name: "foo"}
	fmt.Println(p.Name)
	fmt.Println(p.Hello())

	{
		type W Person
		p := W(p)
		fmt.Println(p.Name)
		// fmt.Println(p.Hello())
	}
}
