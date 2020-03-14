package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"os"
)

// Person ...
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	code := `
{
  "name": "foo",
  "age": 20,
  "nickname": "F"
}
`
	fmt.Println("input:")
	fmt.Println(code)
	fmt.Println("")

	o := bytes.NewBufferString(code)
	decoder := json.NewDecoder(o)

	var ob Person
	if err := decoder.Decode(&ob); err != nil {
		return err
	}

	fmt.Println("got:")
	fmt.Printf("%#+v\n", ob)

	fmt.Println("")
	fmt.Println("output:")
	fmt.Println(code)

	encoder := json.NewEncoder(os.Stdout)
	if err := encoder.Encode(&ob); err != nil {
		return err
	}
	return nil
}
