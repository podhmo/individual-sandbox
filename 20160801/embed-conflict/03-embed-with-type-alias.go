package main

import (
	"encoding/json"
	"fmt"
)

type MyInt int
type Person struct {
	Name string
	Age  int
}

type MyPerson struct {
	*Person
	Age MyInt
}

func (p Person) ForMarshal() MyPerson {
	return MyPerson{Person: &p, Age: MyInt(p.Age)}
}

func main() {
	{
		p := Person{Name: "foo", Age: 20}
		output, _ := json.Marshal(&p)
		fmt.Printf("person: %#v\n", p)
		fmt.Printf("output: %s\n", output)
	}
	fmt.Println("----------------------------------------")
	{
		// this is bad.
		mp := MyPerson{Person: &Person{Name: "foo", Age: 20}}
		output, _ := json.Marshal(&mp)
		fmt.Printf("this is bad. my person: %#v\n", mp)
		fmt.Printf("this is bad. output: %s\n", output)
	}
	fmt.Println("----------------------------------------")
	{
		p := Person{Name: "foo", Age: 20}.ForMarshal()
		mp := p.ForMarshal()
		output, _ := json.Marshal(&mp)
		fmt.Printf("my person: %#v\n", mp)
		fmt.Printf("output: %s\n", output)
	}
}
