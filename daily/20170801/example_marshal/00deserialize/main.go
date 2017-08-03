package main

import (
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
)

type P struct {
	Name string
}

func main() {
	b := []byte(`{"name": "foo"}`)
	var ob P
	if err := json.Unmarshal(b, &ob); err != nil {
		log.Fatal(err)
	}
	pp.Println(ob)
}
