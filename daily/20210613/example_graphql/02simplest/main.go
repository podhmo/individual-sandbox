package main

import (
	"context"
	"fmt"
	"log"
	"strings"

	"github.com/graphql-go/graphql"
	"github.com/k0kubun/pp"
)

type Todo struct {
	ID   string `json:"id"`
	Text string `json:"text"`
	Done bool   `json:"done"`
}

var DB struct {
	TodoList []Todo
}

func init() {
	DB.TodoList = []Todo{
		{ID: "1", Text: "go to bed", Done: true},
		{ID: "2", Text: "eat breakfast", Done: false},
	}
}

// schema
var todoType = graphql.NewObject(graphql.ObjectConfig{
	Name: "Todo",
	Fields: graphql.Fields{
		"id": &graphql.Field{
			Type: graphql.NewNonNull(graphql.String),
		},
		"text": &graphql.Field{
			Type: graphql.String,
		},
		"done": &graphql.Field{
			Type: graphql.Boolean,
		},
		"meta": &graphql.Field{
			Type: graphql.String,
			Resolve: func(p graphql.ResolveParams) (interface{}, error) {
				item := p.Source.(Todo)
				return map[string]string{
					"something": strings.Repeat(item.ID, 10),
				}, nil
			},
		},
	},
})

var rootQuery = graphql.NewObject(graphql.ObjectConfig{
	Name: "RootQuery",
	Fields: graphql.Fields{
		"todoList": &graphql.Field{
			Type:        graphql.NewList(todoType),
			Description: "List of todos",
			Args: graphql.FieldConfigArgument{
				"doneOnly": &graphql.ArgumentConfig{
					Type: graphql.Boolean,
				},
			},
			Resolve: func(p graphql.ResolveParams) (interface{}, error) {
				// rootValue := p.Info.RootValue.(map[string]interface{})

				doneOnly := false
				if v, ok := p.Args["doneOnly"].(bool); ok {
					doneOnly = v
				}

				list := DB.TodoList
				if !doneOnly {
					return list, nil
				}
				r := make([]Todo, 0, len(list))
				for i := 0; i < len(list); i++ {
					if list[i].Done {
						r = append(r, list[i])
					}
				}
				return r, nil
			},
		},
	},
})

var Schema, _ = graphql.NewSchema(graphql.SchemaConfig{
	Query: rootQuery,
})

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func runEndpoint(ctx context.Context, q string) *graphql.Result {
	result := graphql.Do(graphql.Params{
		Schema:        Schema,
		Context:       ctx,
		RequestString: q,
		RootObject: map[string]interface{}{
			"xxx": 111, //
		},
	})
	return result
}

func run() error {
	ctx := context.Background()
	q := `query{ todoList(doneOnly: true){id, text, meta} }`

	result := runEndpoint(ctx, q)
	if result.HasErrors() {
		return fmt.Errorf("failed %#+v", result.Errors)
	}
	pp.Println(result)
	return nil
}
