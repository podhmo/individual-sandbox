package main

import (
	"encoding/json"
	"log"
	"os"
)

// Person :
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

// PersonResponse :
type PersonResponse struct {
	Name     string `json:"name"`
	Age      int    `json:"age"`
	Nickname string `json:"nickname"`
}

// Converter :
type Converter struct {
}

// ConvertPerson :
func (c *Converter) ConvertPerson(src *Person) *PersonResponse {
	if src == nil {
		return nil
	}
	dst := &PersonResponse{
		Name: src.Name,
		Age:  src.Age,
	}
	return dst
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	p := &Person{Name: "foo", Age: 20}
	c := &Converter{}
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(c.ConvertPerson(p))
}
