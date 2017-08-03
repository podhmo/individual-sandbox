package main

import (
	"encoding/json"
	"fmt"
	"log"
)

type P struct {
	Value int `json:"value"`
}

func main() {
	b := []byte(`{}`) // omitempty-ed
	var ob P
	if err := json.Unmarshal(b, &ob); err != nil {
		log.Fatal(err)
	}
	fmt.Println(&ob)
}
