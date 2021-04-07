package main

import (
	"fmt"
	"log"

	"github.com/xeipuuv/gojsonschema"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	// schemaLoader := gojsonschema.NewReferenceLoader("file://schema.json")
	schemaLoader := gojsonschema.NewStringLoader(`
{
  "properties": {
    "name": {
      "type": "string"
    },
    "age": {
      "type": "integer"
    }
  },
  "required": ["name"],
  "additionalProperties": false
}
`)
	schema, err := gojsonschema.NewSchema(schemaLoader)
	if err != nil {
		return err
	}
	result, err := schema.Validate(gojsonschema.NewStringLoader(`{"name": "foo", "xx": "y"}`))

	if !result.Valid() {
		fmt.Printf("The document is not valid. see errors :\n")
		for _, err := range result.Errors() {
			// Err implements the ResultError interface
			fmt.Printf("- %s\n", err)
		}
		return fmt.Errorf("validation error %+v", result.Errors())
	}
	fmt.Println("ok")
	return nil
}
