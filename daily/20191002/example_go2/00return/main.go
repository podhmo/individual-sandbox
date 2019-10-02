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

func create(name string, age int) Person {
	// zero値を色んなところで返す関数なら何でも良い
	if name == "" {
		return Person{}
	}
	if age == 0 {
		return Person{}
	}
	return Person{Name: name, Age: age}
}
