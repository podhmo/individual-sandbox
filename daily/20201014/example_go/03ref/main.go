package main

import "github.com/k0kubun/pp"

type Person struct {
	Name   string
	Father *Person
}

type Ptr struct {
	raw *interface{}
}

func main() {
	var ob interface{}
	m := map[string]interface{}{}
	m["name"] = "string"
	m["father"] = Ptr{raw: &ob}
	m["self"] = Person{Name: "foo"}
	ob = m["self"]
	pp.Println(m)
}
