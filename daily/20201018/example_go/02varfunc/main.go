package main

import (
	"encoding/json"
	"os"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

type Todo struct {
	Title string
}

var ListTodo func() []Todo

func main() {
	s := shape.Extract(ListTodo)
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(s)
}
