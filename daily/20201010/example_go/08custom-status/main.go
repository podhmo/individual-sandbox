package main

import (
	"context"
	"encoding/json"
	"os"

	reflectopenapi "github.com/podhmo/reflect-openapi"
)

func main() {
	c := reflectopenapi.Config{}
	doc, _ := c.BuildDoc(context.Background(), func(m *reflectopenapi.Manager) {
		op := m.Visitor.VisitFunc(func() string { return "" })
		op.Responses["201"] = op.Responses["200"]
		delete(op.Responses, "200")
		m.Doc.AddOperation("/something", "POST", op)
	})
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(doc)
}
