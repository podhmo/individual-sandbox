package main

import (
	"encoding/json"
	"os"

	"github.com/getkin/kin-openapi/openapi3"
)

type Person struct {
	Title string // required
	Age   int
}

func ListPerson() []Person {
	return nil
}

func Emit(ob interface{}) error {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(ob)
}

func main() {
	s := openapi3.NewObjectSchema()
	{
		f := openapi3.NewStringSchema()
		s.Properties["title"] = &openapi3.SchemaRef{Value: f}
	}
	{
		f := openapi3.NewIntegerSchema()
		s.Properties["age"] = &openapi3.SchemaRef{Value: f}
	}
	s.Required = []string{"title"}
	Emit(s)
}
