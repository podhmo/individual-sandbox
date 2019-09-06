package main

import (
	"encoding/json"
	"fmt"
	"log"
)

type Person struct {
	Name   string  `json:"name"` // required
	Age    int     `json:"age"`  // required
	Father *Person `json:"father"`
}

// UnmarshalJSON :
func (p *Person) UnmarshalJSON(b []byte) error {
	type personP struct {
		Name   *string  `json:"name"` // required
		Age    *int     `json:"age"`  // required
		Father **Person `json:"father"`
	}

	var pt personP
	if err := json.Unmarshal(b, &pt); err != nil {
		return err
	}
	if pt.Name == nil {
		return fmt.Errorf("name is not found data=%s", b)
	}
	p.Name = *pt.Name
	if pt.Age == nil {
		return fmt.Errorf("age is not found data=%s", b)
	}
	p.Age = *pt.Age

	if pt.Father != nil {
		p.Father = *pt.Father
	}
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	data := `
{"name": "foo", "age": 20, "father": {"name": "bar"}}
`
	var p Person
	return json.Unmarshal([]byte(data), &p)

}
