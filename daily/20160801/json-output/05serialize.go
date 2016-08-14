package main

// conditional output

import (
	"encoding/json"
	"fmt"
	"time"
)

type MyTime time.Time

type Person struct {
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
	CreatedAt time.Time `json:"created_at"`
}


func (m *MyTime) MarshalJSON() ([]byte, error) {
	return []byte(`"` + m.format() + `"`), nil
}

func (m *MyTime) format() string {
	return (time.Time(*m)).Format("2006-01-02 15:04:05")
}

type OnlyNamePerson Person

func (p OnlyNamePerson) MarshalJSON() ([]byte, error) {
	return json.Marshal(&struct {
		FullName string `json: "fullname"`
	}{
        FullName: p.FirstName + " " + p.LastName,
	})
}

type WithTimestampPerson Person

func (p WithTimestampPerson) MarshalJSON() ([]byte, error) {
	return json.Marshal(&struct {
        LastSeen int64 `json: "last_seen"`
        Person
	}{
        Person: Person(p), LastSeen: time.Now().Unix(),
	})
}

func main() {
	{
		person := Person{FirstName: "foo", LastName: "bar", CreatedAt: time.Now()}
		output, _ := json.Marshal(OnlyNamePerson(person))
		fmt.Printf("output: %s\n", output)
	}
	{
		person := Person{FirstName: "foo", LastName: "bar", CreatedAt: time.Now()}
		output, _ := json.Marshal(WithTimestampPerson(person))
		fmt.Printf("output: %s\n", output)
	}
}
