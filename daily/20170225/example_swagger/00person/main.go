package main

import (
	"fmt"

	"github.com/foo/foo/models"
	"github.com/go-openapi/strfmt"
)

func main() {
	registry := strfmt.NewFormats()
	person := models.Person{}
	if _, err := registry.Parse(person, "{}"); err != nil {
		panic(err)
	}
	fmt.Println("yay")
}
