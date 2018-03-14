package main

import (
	"encoding/json"
	"fmt"
	"os"

	"gopkg.in/mgo.v2/bson"
)

// S :
type S struct {
	Name        string         `json:"name" bson:"name"`
	Additionals map[string]int `json:",inline" bson:",inline"`
}

func main() {
	s := S{Name: "foo", Additionals: map[string]int{"xxx": 10}}
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	encoder.Encode(&s)
	fmt.Println("----------------------------------------")
	b, _ := bson.MarshalJSON(&s)
	fmt.Println(string(b))
}
