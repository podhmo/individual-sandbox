package main

import (
	"log"
	"m/glue"
	"m/glue/clientgen"
)

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

type ListInput struct {
	Offset int `openapi:"query"`
	Limit  int `openapi:"query"`
}

func main() {
	b := Build()
	c := &clientgen.Config{}
	_ = b
	if err := c.Gen(b); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Build() *glue.Glue {
	g := glue.NewGlue()

	g.Group("todo", func(b *glue.Glue) {
		g.Operation("ListInput", func(input struct {
			ListInput
		}) []Todo {
			return nil
		})
	})
	return g
}
