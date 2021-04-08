package main

import (
	"log"
	"os"

	"github.com/podhmo/validator/schemagen"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Person struct {
	Name     string           `json:"name" validate:"@foo"`
	Age      int              `json:"age"`
	Children []*Person        `json:"children"`
	Items    []map[string]int `json:"items"`

	Messages []Message
}

type Message struct {
	Title string
}

func run() error {
	c := schemagen.Config{}
	return c.OpenAPI(os.Stdout, Person{})
}
