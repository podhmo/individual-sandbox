package main

import (
	"reflect"

	reflectopenapi "github.com/podhmo/reflect-openapi"
)

func main() {
	c := reflectopenapi.Config{
		SkipValidation: true,
	}
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		rt := reflect.StructOf([]reflect.StructField{
			{
				Name: "Name",
				Type: reflect.TypeOf(""),
			},
		})
		m.Visitor.VisitType(reflect.New(rt).Interface())
	})
}
