package main

// sorting custom type

import (
	"fmt"
	"sort"
)

type person struct {
	name string
	age  int
}

type byname []person
type byage []person

func (ps byname) Len() int {
	return len(ps)
}
func (ps byname) Less(i, j int) bool {
	return ps[i].name <= ps[j].name
}
func (ps byname) Swap(i, j int) {
	ps[i], ps[j] = ps[j], ps[i]
}

func (ps byage) Len() int {
	return len(ps)
}
func (ps byage) Less(i, j int) bool {
	return ps[i].age <= ps[j].age
}
func (ps byage) Swap(i, j int) {
	ps[i], ps[j] = ps[j], ps[i]
}

func main() {
	foo := person{name: "foo", age: 10}
	bar := person{name: "bar", age: 20}
	boo := person{name: "boo", age: 5}
	xs := []person{foo, bar, boo}

	{
		fmt.Printf("default: %v\n", xs)
	}
	{
		fmt.Println("----------------------------------------")
		sort.Sort(byname(xs))
		fmt.Printf("sort by name: %v\n", xs)
		sort.Sort(sort.Reverse(byname(xs)))
		fmt.Printf("rsort by name: %v\n", xs)
	}
	{
		fmt.Println("----------------------------------------")
		sort.Sort(byage(xs))
		fmt.Printf("sort by age: %v\n", xs)
		sort.Sort(sort.Reverse(byage(xs)))
		fmt.Printf("rsort by age: %v\n", xs)
	}
}
