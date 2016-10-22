package main

import (
	"encoding/json"
	"log"
)

type Person struct {
	ID   string `json:"id"`
	Name string `json:"name"`
	Age  int    `json:"age"`

	// for population
	Info *PersonInfo `json:"-"`
}

type PersonInfo struct {
	PersonID string `json:"personID"`
	Content  string `json:"content"`
}

func main() {
	person := &Person{ID: "1", Name: "foo", Age: 20, Info: &PersonInfo{PersonID: "1", Content: "yay"}}

	{
		// onesself only
		b, err := json.Marshal(person)
		if err != nil {
			log.Fatal(err)
		}
		log.Println(string(b))
		// {"id":"1","name":"foo","age":20}
	}

	{
		// with info
		type M struct {
			*Person
			Info *PersonInfo
		}
		b, err := json.Marshal(&M{Person: person, Info: person.Info})
		if err != nil {
			log.Fatal(err)
		}
		log.Println(string(b))
		// {"id":"1","name":"foo","age":20,"Info":{"personID":"1","content":"yay"}}
	}
	{
		// with info, inline
		type M struct {
			*Person
			*PersonInfo
		}
		b, err := json.Marshal(&M{Person: person, PersonInfo: person.Info})
		if err != nil {
			log.Fatal(err)
		}
		log.Println(string(b))
		// {"id":"1","name":"foo","age":20,"personID":"1","content":"yay"}
	}
	{
		// with info, inline, no personID
		type _N struct {
			*PersonInfo
			PersonID string `json:"personID,omitempty"`
		}
		type M struct {
			*Person
			*_N
		}
		b, err := json.Marshal(&M{Person: person, _N: &_N{PersonInfo: person.Info}})
		if err != nil {
			log.Fatal(err)
		}
		log.Println(string(b))
		// {"id":"1","name":"foo","age":20,"content":"yay"}
	}
}
