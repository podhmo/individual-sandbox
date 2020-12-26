package main

import reflectopenapi "github.com/podhmo/reflect-openapi"

type S struct {
	Data Json
}

type Json struct {
	data interface{}
}

func main() {
	c := reflectopenapi.Config{}
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		m.Visitor.VisitType(S{})
	})
}
