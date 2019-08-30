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
		Name   *string          `json:"name"` // required
		Age    *int             `json:"age"`  // required
		Father *json.RawMessage `json:"father"`
	}

	var ref personP
	if err := json.Unmarshal(b, &ref); err != nil {
		return err
	}

	if ref.Name == nil {
		return fmt.Errorf("name is not found data=%s", b)
	}
	p.Name = *ref.Name

	if ref.Age == nil {
		return fmt.Errorf("age is not found data=%s", b)
	}
	p.Age = *ref.Age

	if ref.Father != nil {
		if p.Father == nil {
			p.Father = &Person{}
		}
		if err := json.Unmarshal(*ref.Father, p.Father); err != nil {
			return fmt.Errorf("father/%s", err.Error())
		}
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
