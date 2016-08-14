package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
}

type MyPerson Person

func (p MyPerson) MarshalJSON() ([]byte, error) {
	return json.Marshal(&struct {
		FullName string `json:"fullname"`
	}{
		FullName: p.FirstName + " " + p.LastName,
	})
}

type MyPerson2 Person

func (p *MyPerson2) MarshalJSON() ([]byte, error) {
	return json.Marshal(&struct {
		FullName string `json:"full_name"`
	}{
		FullName: p.FirstName + " " + p.LastName,
	})
}

func main() {
	{
		{
			person := MyPerson{FirstName: "foo", LastName: "bar"}
			{
				output, _ := json.Marshal(person)
				fmt.Printf("my person: %s\n", output)
			}
			{
				output, _ := json.Marshal(&person)
				fmt.Printf("my person*: %s\n", output)
			}
		}
        fmt.Println("----------------------------------------")
		{
			person := MyPerson2{FirstName: "foo", LastName: "bar"}
			{
				output, _ := json.Marshal(person)
				fmt.Printf("my person: %s\n", output)
			}
			{
				output, _ := json.Marshal(&person)
				fmt.Printf("my person*: %s\n", output)
			}
		}
	}
}
