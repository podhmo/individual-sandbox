package main

import (
	"github.com/grafana/grafana/pkg/models"
	reflectopenapi "github.com/podhmo/reflect-openapi"
)

func main() {
	var c reflectopenapi.Config

	c.EmitDoc(func(m *reflectopenapi.Manager) {
		m.Visitor.VisitType(models.AlertListItemDTO{})
	})
}
