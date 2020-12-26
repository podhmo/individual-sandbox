package main

import reflectopenapi "github.com/podhmo/reflect-openapi"

type S struct {
	unexported            string
	unexportedWithTag     string `json:"unexported"`
	unexportedWithTagJSON string `json:"-"`
}

type Zero struct {
}

type HasInterface struct {
	I interface{}
}

type HasUnexportedInterface struct {
	unexported interface{}
}

func main() {
	c := reflectopenapi.Config{}
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		m.Visitor.VisitType(S{})
		m.Visitor.VisitType(Zero{})
		m.Visitor.VisitType(HasInterface{})
		m.Visitor.VisitType(HasUnexportedInterface{})
	})
}
