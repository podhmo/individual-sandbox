package main

import (
	"encoding/json"
	"fmt"
	"log"
)

type Child1 struct {
	Id   string
	Name string
}

type Child2 struct {
	Id    int
	Value int
}

type Person struct {
	Child1
	Child2
}

func main() {
	{
		p := Person{Child1: Child1{Id: "p1", Name: "foo"}, Child2: Child2{Id: 2, Value: 10}}
		fmt.Printf("person: %#v\n", p)
		output, err := json.Marshal(&p)
		if err != nil {
			log.Panicf("invalid %v", err)
		}
		fmt.Printf("output: %s\n", output)
	}
	{
		fmt.Println("----------------------------------------")
		output, err := json.Marshal(&(struct{ Child2 }{Child2: Child2{Id: 2, Value: 10}}))
		if err != nil {
			log.Panicf("invalid %v", err)
		}
		fmt.Printf("output: %s\n", output)

	}
}
