package main

import (
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
)

func main() {
	val := make(map[string]json.RawMessage)
	b := []byte(`{"k": 100}`)
	if err := json.Unmarshal(b, &val); err != nil {
		log.Fatal(err)
	}
	pp.Println(val)
	var i *int64
	if err := json.Unmarshal(val["k"], &i); err != nil {
		log.Fatal(err)
	}
	pp.Println(*i)
}
