package main

import (
	"encoding/json"
	"fmt"
	"os"

	"github.com/davecgh/go-spew/spew"
)

// Person :
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

// Person2 :
type Person2 struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")

	p := Person{Name: "foo", Age: 20}
	p2 := Person2{Name: "foo", Age: 20}

	myEncode := func(o interface{}) {
		encoder.Encode(o)
	}

	{
		fmt.Println("-struct---------------------------------------")
		encoder.Encode(&p)
	}
	{
		fmt.Println("-interface---------------------------------------")
		myEncode(&p)
	}
	{
		fmt.Println("-interface2---------------------------------------")
		myEncode(&p2)
	}
	{
		fmt.Println("-convert----------------------------------------")
		myDecode := func(b []byte, o interface{}) {
			json.Unmarshal(b, o)
		}
		s, _ := json.Marshal(&p)
		var p3 Person2
		myDecode(s, &p3)
		spew.Dump(p3)
	}
}
