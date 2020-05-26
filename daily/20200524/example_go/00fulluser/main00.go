package main

import (
	"encoding/json"
	"os"
)

type User struct {
	ID     int    `json:"id"`
	TeamID int    `json:"teamId"`
	Name   string `json:"name"`
	Age    int    `json:"age"`
}

type Team struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

type UserOutput struct {
	ID   int         `json:"id"`
	Name string      `json:"name"`
	Age  int         `json:"age"`
	Team *TeamOutput `json:"team,omitempty"`
}

type TeamOutput struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

func main() {
	user := User{Name: "foo", Age: 20}
	team := Team{Name: "xxx"}

	output := UserOutput{
		ID:   user.ID,
		Name: user.Name,
		Age:  user.Age,
		Team: &TeamOutput{
			ID:   team.ID,
			Name: team.Name,
		},
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(&output); err != nil {
		panic(err)
	}
}
