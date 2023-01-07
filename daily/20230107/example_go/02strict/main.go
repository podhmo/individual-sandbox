package main

import (
	"bytes"
	"encoding/json"
	"fmt"
)

type S struct {
	Name string `json:"name"`
}

func main() {
	{
		code := `{"name": "foo"}`
		dec := json.NewDecoder(bytes.NewBufferString(code))
		var ob S
		if err := dec.Decode(&ob); err != nil {
			panic(err)
		}
		fmt.Println(ob)
	}
	fmt.Println("----------------------------------------")
	{
		code := `{"name": "foo", "age": 20}`
		dec := json.NewDecoder(bytes.NewBufferString(code))
		dec.DisallowUnknownFields()
		var ob S
		if err := dec.Decode(&ob); err != nil {
			panic(err)
		}
		fmt.Println(ob)
	}
}
