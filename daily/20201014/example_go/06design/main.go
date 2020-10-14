package main

import (
	reflectopenapi "github.com/podhmo/reflect-openapi"
)

type PingInput struct {
	Message string `json:"message" required:"true"`
	N       int    `json:"n" required:"false"`
}

type PingOutput struct {
	Message string `json:"message" required:"true"`
}

func DoPing(input PingInput) (*PingOutput, error) {
	return nil, nil
}

func main() {
	var c reflectopenapi.Config
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		op := m.Visitor.VisitFunc(DoPing)
		m.Doc.AddOperation("/ping", "GET", op)
	})
}
