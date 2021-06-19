package schema

import (
	"m/01todo-separated/schema/model"
	"m/01todo-separated/schema/storage"

	"github.com/graphql-go/graphql"
)

func getTodo() *graphql.Field {
	return &graphql.Field{
		Type:        todoType,
		Description: "Get single todo",
		Args: graphql.FieldConfigArgument{
			"id": &graphql.ArgumentConfig{
				Type: graphql.String,
			},
		},
		Resolve: func(params graphql.ResolveParams) (interface{}, error) {

			idQuery, isOK := params.Args["id"].(string)
			if isOK {
				// Search for el with id
				for _, todo := range storage.TodoList {
					if todo.ID == idQuery {
						return todo, nil
					}
				}
			}

			return model.Todo{}, nil
		},
	}
}

func lastTodo() *graphql.Field {
	return &graphql.Field{
		Type:        todoType,
		Description: "Last todo added",
		Resolve: func(params graphql.ResolveParams) (interface{}, error) {
			return storage.TodoList[len(storage.TodoList)-1], nil
		},
	}
}

func todoList() *graphql.Field {
	return &graphql.Field{
		Type:        graphql.NewList(todoType),
		Description: "List of todos",
		Resolve: func(p graphql.ResolveParams) (interface{}, error) {
			return storage.TodoList, nil
		},
	}
}
