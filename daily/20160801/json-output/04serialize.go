package main

// change time.Time layout

import (
	"encoding/json"
	"fmt"
	"time"
)

type MyTime time.Time

type Person struct {
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
	CreatedAt MyTime `json:"created_at"`
}

func (m *MyTime) MarshalJSON() ([]byte, error) {
	return []byte(`"` + m.format() + `"`), nil
}

func (m *MyTime) format() string {
	return (time.Time(*m)).Format("2006-01-02 15:04:05")
}

func main() {
	{
		person := Person{FirstName: "foo", LastName: "bar", CreatedAt: MyTime(time.Now())}
		output, _ := json.Marshal(&person)
		fmt.Printf("output: %s\n", output)
	}
}
