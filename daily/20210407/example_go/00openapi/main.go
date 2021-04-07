package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"

	"github.com/getkin/kin-openapi/openapi3"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	loader := openapi3.NewSwaggerLoader()
	doc, err := loader.LoadSwaggerFromData([]byte(`
openapi: 3.1.0
info:
  version: 0.0.0
  title: my api
components:
  schemas:
    person:
      properties:
        name:
          type: string
        age:
          type: integer
      required:
        - name
      additionalProperties: false
paths: {}
`))
	if err != nil {
		return err
	}

	if err := doc.Validate(context.Background()); err != nil {
		return err
	}

	is, err := doc.Components.Schemas.JSONLookup("person")
	if err != nil {
		return err
	}
	s, ok := is.(*openapi3.Schema)
	if !ok {
		return fmt.Errorf("unexpected type %T", is)
	}
	var v interface{}
	if err := json.Unmarshal([]byte(`{"name": "foo", "age": 10, "x-xxx": "x"}`), &v); err != nil {
		return err
	}
	if err := s.VisitJSON(v); err != nil {
		return err
	}

	fmt.Println(v)
	return nil
}
