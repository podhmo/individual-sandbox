package main

import (
	"encoding/json"
	"fmt"

	"github.com/k0kubun/pp"
)

func main() {
	{
		d := `{}`
		var p Person
		fmt.Println("input", d)
		fmt.Println(json.Unmarshal([]byte(d), &p))
	}
	fmt.Println("----------------------------------------")
	{
		d := `{"name": "foo", "age": 10, "createdAt": "2018-01-01T00:00:00Z"}`
		var p Person
		fmt.Println("input", d)
		fmt.Println(json.Unmarshal([]byte(d), &p))
	}
	fmt.Println("----------------------------------------")
	{
		d := `{"name": "foo", "age": 10, "createdAt": "2018-01-01T00:00:00Z", "father": {}}`
		var p Person
		fmt.Println("input", d)
		err := json.Unmarshal([]byte(d), &p)
		fmt.Printf("%+v\n, val=%+v\n", err, p)
	}
	fmt.Println("----------------------------------------")
	{
		d := `{"name": "foo", "age": 10, "createdAt": "2018-01-01T00:00:00Z", "father": {"name": "foo", "age": -10,"createdAt": "2018-01-01T00:00:00Z"}}`
		var p Person
		fmt.Println("input", d)
		err := json.Unmarshal([]byte(d), &p)
		pp.Println(err)
		fmt.Printf("%+v\n, val=%+v\n", err, p)
	}
}
