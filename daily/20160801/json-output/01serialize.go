package main

import (
	"encoding/json"
	"fmt"
	"time"
)

type Person struct {
	FirstName string
	LastName  string
	CreatedAt time.Time
}

func main() {
	{
		person := Person{FirstName: "foo", LastName: "bar", CreatedAt: time.Now()}
		output, _ := json.Marshal(&person)
		fmt.Printf("output: %s\n", output)

	}
}
