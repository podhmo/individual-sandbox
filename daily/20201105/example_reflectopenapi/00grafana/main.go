package main

import (
	"github.com/grafana/grafana/pkg/models"
	reflectopenapi "github.com/podhmo/reflect-openapi"
)

// https://github.com/grafana/grafana/blob/master/pkg/api/api.go#L349
// https://github.com/grafana/grafana/blob/62138e8ad482f30e159ea5984516d1c593237d2b/pkg/api/alerting.go#L52
// https://github.com/grafana/grafana/blob/62138e8ad482f30e159ea5984516d1c593237d2b/pkg/models/alert.go#L184

func main() {
	var c reflectopenapi.Config

	// apiRoute.Group("/alerts", func(alertsRoute routing.RouteRegister) {
	// 	alertsRoute.Post("/test", bind(dtos.AlertTestCommand{}), Wrap(AlertTest))
	// 	alertsRoute.Post("/:alertId/pause", reqEditorRole, bind(dtos.PauseAlertCommand{}), Wrap(PauseAlert))
	// 	alertsRoute.Get("/:alertId", ValidateOrgAlert, Wrap(GetAlert))
	// 	alertsRoute.Get("/", Wrap(GetAlerts))
	// 	alertsRoute.Get("/states-for-dashboard", Wrap(GetAlertStatesForDashboard))
	// })

	c.EmitDoc(func(m *reflectopenapi.Manager) {
		op := m.Visitor.VisitFunc(GetAlerts)
		m.Doc.AddOperation("/alerts/", "GET", op)
	})
}

func GetAlerts() []*models.AlertListItemDTO {
	return nil
}
