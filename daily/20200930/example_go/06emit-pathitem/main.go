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
	// TODO: validate
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

	doc := &openapi3.Swagger{}
	{
		op := openapi3.NewOperation()
		op.OperationID = "main.GetPerson"
		op.Responses = openapi3.NewResponses()
		{
			op.Responses["200"] = &openapi3.ResponseRef{
				Value: openapi3.NewResponse().WithDescription("").WithJSONSchema(s),
			}
		}
		doc.AddOperation("/people", "GET", op)
	}
	Emit(doc)
}
