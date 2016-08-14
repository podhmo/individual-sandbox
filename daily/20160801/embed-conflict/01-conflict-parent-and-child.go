package main

import (
	"encoding/json"
	"fmt"
	"log"
)

type Person struct {
	Id   string
	Name string
	Age  int
}

type WrappedPerson struct {
	Id int
	Person
}

func dump(p interface{}) {
	output, err := json.Marshal(p)
	if err != nil {
		log.Panicf("invalid %#v", err)
	}
	fmt.Printf("output: %s\n", output)
}

func main() {
	person := Person{Id: "p1", Name: "Foo", Age: 20}
	wPerson := WrappedPerson{Id: 1, Person: person}
	fmt.Printf("person = %#v\n", person)
	dump(&person)
	fmt.Println("----------------------------------------")
	fmt.Printf("wrapped person = %#v\n", wPerson)
	dump(&wPerson)
}
