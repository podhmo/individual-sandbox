package main

import (
	"encoding/json"
	"os"
)

type PutPersonInput2 struct {
	PersonID string `json:"personId"`
	Person          // inline
}
type Person struct {
	Name string `json:"name"` // required
	Age  int    `json:"age"`
}

func main() {
	p := PutPersonInput2{
		PersonID: "x",
		Person: Person{
			Name: "foo",
			Age:  20,
		},
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(p)
}
