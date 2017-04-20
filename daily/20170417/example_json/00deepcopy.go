package main

import (
	"encoding/json"
	"log"

	"github.com/davecgh/go-spew/spew"
)

// Person :
type Person struct {
	Name   string  `json:"name"`
	Age    int     `json:"age"`
	Mother *Person `json:"mother"`
	Father *Person `json:"father"`
}

// DeepCopy :
func DeepCopy(src interface{}, dst interface{}) error {
	b, err := json.Marshal(src)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(b, dst); err != nil {
		return err
	}
	return nil
}

func main() {
	person := Person{
		Name:   "foo",
		Age:    20,
		Mother: &Person{Name: "boo", Age: 40},
		Father: &Person{Name: "bee", Age: 40},
	}
	// original
	spew.Dump(person)

	// copy
	{
		var person2 Person
		b, err := json.Marshal(&person)
		if err != nil {
			log.Fatal(err)
		}
		if err := json.Unmarshal(b, &person2); err != nil {
			log.Fatal(err)
		}
		spew.Dump(person2)
	}

	// deepcopy
	{
		var person3 Person
		if err := DeepCopy(&person, &person3); err != nil {
			log.Fatal(err)
		}
		spew.Dump(person3)
	}
}
