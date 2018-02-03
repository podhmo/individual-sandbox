package main

import (
	"encoding/json"
	"os"

	"./internal"
)

func main() {
	p := internal.Person{
		ID:       1,
		Name:     "akane",
		Birthday: "08-16",
		VividInfo: internal.VividInfo{
			Color:  "red",
			Weapon: "Rang",
		},
	}
	encoder := json.NewEncoder(os.Stdout)
	encoder.Encode(&p)
}
