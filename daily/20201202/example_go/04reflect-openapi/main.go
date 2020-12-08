package main

import (
	"context"

	reflectopenapi "github.com/podhmo/reflect-openapi"
)

type Person struct {
	Name string `json:"name" validate:"required"`
	Age  int    `json:"age" validate:"required"`
}

func ListPerson(ctx context.Context) ([]Person, error) {
	return nil, nil
}

func main() {
	c := reflectopenapi.Config{}
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		op := m.Visitor.VisitFunc(ListPerson)
		m.Doc.AddOperation("ListPerson", "POST", op)
	})
}
