package main

import (
	"encoding/json"
	"os"

	reflectopenapi "github.com/podhmo/reflect-openapi"
)

type APIError struct {
	Status  int      `json:"status"`
	Message string   `json:"message"`
	Details []string `json:"details"`
}

func main() {
	r := &reflectopenapi.NoRefResolver{}
	v := reflectopenapi.NewVisitor(r)
	s := v.VisitType(APIError{})

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(s)
}
