package main

import (
	"bytes"
	"encoding/json"
	"fmt"
)

type Person struct {
	Name string `json:"name"`
}

func main() {
	{
		var p Person
		dec := json.NewDecoder(bytes.NewBufferString(`{"name": "foo"}`))
		fmt.Println(dec.Decode(&p))
		fmt.Println(p)
	}
	{
		var p Person
		dec := json.NewDecoder(bytes.NewBufferString(`{"name": "foo", "age": 20}`))
		fmt.Println(dec.Decode(&p))
		fmt.Println(p)
	}
	{
		var p Person
		dec := json.NewDecoder(bytes.NewBufferString(`{"name": "foo", "age": 20}`))
		dec.DisallowUnknownFields()
		fmt.Println(dec.Decode(&p))
		fmt.Println(p)
	}
}
