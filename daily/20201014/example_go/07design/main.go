package main

import (
	reflectopenapi "github.com/podhmo/reflect-openapi"
	"github.com/stripe/stripe-go/invoice"
)

func main() {
	c := reflectopenapi.Config{
		SkipValidation: true,
	}
	c.EmitDoc(func(m *reflectopenapi.Manager) {
		op := m.Visitor.VisitFunc(invoice.New)
		m.Doc.AddOperation("/invoices", "POST", op)
	})
}
