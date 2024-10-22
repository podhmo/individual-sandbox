package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strings"

	"github.com/perimeterx/marshmallow"
)

// https://pkg.go.dev/github.com/perimeterx/marshmallow

type Person struct {
	Name string `json:"name,omitempty"`
	Age  int    `json:"age,omitempty"`
}

func main() {
	// additionalPeroperties: nickname
	s := `{"name": "foo", "age": 20, "nickname": "F"}`
	{
		var ob Person
		if err := json.NewDecoder(strings.NewReader(s)).Decode(&ob); err != nil {
			log.Fatalf("!! %+v", err)
		}
		fmt.Printf("with json.Decoder:\n\tob=%#+v\n", ob)
	}
	{
		var ob Person
		m, err := marshmallow.Unmarshal([]byte(s), &ob, marshmallow.WithExcludeKnownFieldsFromMap(true))
		if err != nil {
			log.Fatalf("!! %+v", err)
		}
		fmt.Printf("with marshmallow.Unmarshal:\n\tob=%#+v\n\tretval=%#+v\n", ob, m)
	}
}

// Output:
// with json.Decoder:
// 	ob=main.Person{Name:"foo", Age:20}
// with marshmallow.Unmarshal:
// 	ob=main.Person{Name:"foo", Age:20}
// 	retval=map[string]interface {}{"nickname":"F"}
