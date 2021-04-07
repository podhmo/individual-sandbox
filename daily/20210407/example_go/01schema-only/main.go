package main

import (
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
	s := openapi3.NewSchema().
		WithProperty("name", openapi3.NewStringSchema()).
		WithProperty("age", openapi3.NewIntegerSchema())
	s.Required = []string{"name"}
	ng := false
	s.AdditionalPropertiesAllowed = &ng
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
