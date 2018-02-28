package main

import (
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
	"github.com/podhmo/sandbox/person"
)

func main() {
	// s := `{"age": "foo"}`
	s := `{}`
	var p person.Person
	if err := json.Unmarshal([]byte(s), &p); err != nil {
		log.Fatalf("%+v\n", err)
	}
	pp.ColoringEnabled = false
	pp.Println(p)
}

/*
person.Person{
  Name:   "",
  Age:    0,
  Father: (*person.Person)(nil),
  Mother: (*person.Person)(nil),
}
*/
