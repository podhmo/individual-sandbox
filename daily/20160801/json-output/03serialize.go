package main

// omit value (CreatedAt)

import (
	"encoding/json"
	"fmt"
	"time"
)

type Person struct {
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
	CreatedAt time.Time `json:"-"`
}

func main() {
	{
		person := Person{FirstName: "foo", LastName: "bar", CreatedAt: time.Now()}
		output, _ := json.Marshal(&person)
		fmt.Printf("output: %s\n", output)

	}
}
