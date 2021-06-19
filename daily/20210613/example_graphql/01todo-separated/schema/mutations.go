package schema

import (
	"m/01todo-separated/schema/model"
	"m/01todo-separated/schema/storage"

	"github.com/graphql-go/graphql"
)

func createTodo() *graphql.Field {
	return &graphql.Field{
		Type:        todoType, // the return type for this field
		Description: "Create new todo",
		Args: graphql.FieldConfigArgument{
			"text": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
		},
		Resolve: func(params graphql.ResolveParams) (interface{}, error) {

			// marshall and cast the argument value
			text, _ := params.Args["text"].(string)

			// figure out new id
			newID := model.RandStringRunes(8)

			// perform mutation operation here
			// for e.g. create a Todo and save to DB.
			newTodo := model.Todo{
				ID:   newID,
				Text: text,
				Done: false,
			}

			storage.TodoList = append(storage.TodoList, newTodo)

			// return the new Todo object that we supposedly save to DB
			// Note here that
			// - we are returning a `Todo` struct instance here
			// - we previously specified the return Type to be `todoType`
			// - `Todo` struct maps to `todoType`, as defined in `todoType` ObjectConfig`
			return newTodo, nil
		},
	}
}

func updateTodo() *graphql.Field {
	return &graphql.Field{
		Type:        todoType, // the return type for this field
		Description: "Update existing todo, mark it done or not done",
		Args: graphql.FieldConfigArgument{
			"done": &graphql.ArgumentConfig{
				Type: graphql.Boolean,
			},
			"id": &graphql.ArgumentConfig{
				Type: graphql.NewNonNull(graphql.String),
			},
		},
		Resolve: func(params graphql.ResolveParams) (interface{}, error) {
			// marshall and cast the argument value
			done, _ := params.Args["done"].(bool)
			id, _ := params.Args["id"].(string)
			affectedTodo := model.Todo{}

			// Search list for todo with id and change the done variable
			for i := 0; i < len(storage.TodoList); i++ {
				if storage.TodoList[i].ID == id {
					storage.TodoList[i].Done = done
					// Assign updated todo so we can return it
					affectedTodo = storage.TodoList[i]
					break
				}
			}
			// Return affected todo
			return affectedTodo, nil
		},
	}
}
