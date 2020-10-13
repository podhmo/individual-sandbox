package main

import (
	"reflect"

	"github.com/k0kubun/pp"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	{
		ob := Person{Name: "foo", Age: 20}
		pp.Println(ob)
	}
	{
		rPerson := reflect.StructOf([]reflect.StructField{
			{
				Name: "Name",
				Type: reflect.TypeOf(""),
				Tag:  `json:"name"`,
			},
			{
				Name: "Age",
				Type: reflect.TypeOf(int(0)),
				Tag:  `json:"age"`,
			},
		})
		ob := reflect.New(rPerson).Interface()
		pp.Println(ob)
	}
}
