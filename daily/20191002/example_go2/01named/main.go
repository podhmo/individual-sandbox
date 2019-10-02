package main

import "fmt"

type Person struct {
	Name string
	Age  int
}

func main() {
	fmt.Println(create("foo", 20))
	fmt.Println(create("", 20))
}

func create(name string, age int) (p Person) {
	// zero値を色んなところで返す関数なら何でも良い
	if name == "" {
		return p
	}
	if age == 0 {
		return p
	}
	return Person{Name: name, Age: age}
}
