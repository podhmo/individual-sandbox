package main

import (
	"bytes"
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
)

func main() {
	var ob struct {
		Kind string `json:"$kind"`
		json.RawMessage
	}

	b := bytes.NewBufferString(`{"$kind": "atom", "type": "string"}`)
	decoder := json.NewDecoder(b)

	if err := decoder.Decode(&ob); err != nil {
		log.Fatalf("%+v", err)
	}
	pp.Println(ob, string(ob.RawMessage))
}
