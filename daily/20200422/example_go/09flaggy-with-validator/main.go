package main

import (
	"fmt"
	"log"
	"os"

	"github.com/go-playground/validator/v10"
	"github.com/integrii/flaggy"
)

var Option struct {
	Name string `validate:"required"`
}

func main() {
	parser := flaggy.NewParser("app")
	parser.Description = "-- app --"

	parser.String(&Option.Name, "n", "name", "name of person")

	if err := parser.ParseArgs(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}

	validate := validator.New()
	if err := validate.Struct(&Option); err != nil {
		log.Fatalf("!!!%+v", err)
	}

	fmt.Println("@")
}
