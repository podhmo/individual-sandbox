package main

import (
	"github.com/grafana/grafana/pkg/api/dtos"
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
		// https://grafana.com/docs/grafana/latest/http_api/alerting/
		// GET /api/alerts
		{
			op := m.Visitor.VisitFunc(GetAlerts)
			m.Doc.AddOperation("/api/alerts/", "GET", op)
		}
		// POST /api/alerts/test
		{
			op := m.Visitor.VisitFunc(AlertTest)
			m.Doc.AddOperation("/api/alerts/test", "POST", op)
		}
		// GET /api/alerts/:id
		// POST /api/alert-notifications/test
		// POST /api/alerts/:alertId/pause
		// POST /api/admin/pause-all-alerts
	})
}

func GetAlerts(params struct {
	DashboardID    int64    `json:"dashboardId" openapi:"query"`
	PanelID        int64    `json:"panelId" openapi:"query"`
	Query          string   `json:"query" openapi:"query"`
	State          []string `json:"state" openapi:"query"` // ALL, no_data, paused, alerting, ok, pending // multi ?state=paused&state=alerting
	Limit          int64    `json:"limit" openapi:"query"`
	FolderID       int64    `json:"folderId" openapi:"query"`
	DashboardQuery string   `json:"dashboardQuery" openapi:"query"`
	DashboardTag   []string `json:"dashboardTag" openapi:"query"` // multi
}) []*models.AlertListItemDTO {
	// HTTP/1.1 200
	// Content-Type: application/json

	// [
	//   {
	//     "id": 1,
	//     "dashboardId": 1,
	//     "dashboardUId": "ABcdEFghij"
	//     "dashboardSlug": "sensors",
	//     "panelId": 1,
	//     "name": "fire place sensor",
	//     "state": "alerting",
	//     "newStateDate": "2018-05-14T05:55:20+02:00",
	//     "evalDate": "0001-01-01T00:00:00Z",
	//     "evalData": null,
	//     "executionError": "",
	//     "url": "http://grafana.com/dashboard/db/sensors"
	//   }
	// ]
	return nil
}

func AlertTest(cmd *dtos.AlertTestCommand) *dtos.AlertTestResult {
	return nil
}
