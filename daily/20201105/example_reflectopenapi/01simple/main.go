package main

import (
	"time"

	reflectopenapi "github.com/podhmo/reflect-openapi"
)

type Media struct {
	Name  string    `json:"name"`
	CTime time.Time `json:"ctime"`
}

func main() {
	var c reflectopenapi.Config

	c.EmitDoc(func(m *reflectopenapi.Manager) {
		op := m.Visitor.VisitFunc(ListMedia)
		m.Doc.AddOperation("/Media/", "GET", op)
	})
}

func ListMedia() []*Media {
	return nil
}
