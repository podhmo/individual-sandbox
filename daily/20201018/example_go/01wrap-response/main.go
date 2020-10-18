package main

import (
	"reflect"

	reflectopenapi "github.com/podhmo/reflect-openapi"
	"github.com/podhmo/reflect-openapi/pkg/shape"
)

type Person struct {
	Name string
}

func ListPerson() []Person {
	return nil
}
func GetPerson(params struct {
	ID string `json:"id" openapi:"path"`
}) *Person {
	return nil
}

type CustomSelector struct {
	reflectopenapi.Selector
	Extractor reflectopenapi.Extractor
}

// wrap with {"items": <>}
func (s *CustomSelector) SelectOutput(fn shape.Function) shape.Shape {
	out := s.Selector.SelectOutput(fn)
	if out, ok := out.(shape.Container); ok && out.GetReflectKind() == reflect.Slice {
		rt := reflect.StructOf([]reflect.StructField{
			{
				Name: "Items",
				Type: out.GetReflectType(),
				Tag:  `json:"items"`,
			},
		})
		return s.Extractor.Extract(reflect.New(rt).Interface())
	}
	return out
}

func main() {
	c := reflectopenapi.Config{
		SkipValidation: true,
	}
	c.Selector = &CustomSelector{
		Selector:  c.DefaultSelector(),
		Extractor: c.DefaultExtractor(),
	}
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		{
			op := m.Visitor.VisitFunc(ListPerson)
			m.Doc.AddOperation("/todo", "GET", op)
		}
		{
			op := m.Visitor.VisitFunc(GetPerson)
			m.Doc.AddOperation("/todo/{id}", "GET", op)
		}
	})
}
