package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"m/01todo-separated/schema"

	"github.com/graphql-go/handler"
)

// open http://localhost:8888/graphql
//
// # Write your query or mutation here
// query {
//   todoList {
//     text
//   }
// }

func main() {
	h := handler.New(&handler.Config{
		Schema:     &schema.TodoSchema,
		Pretty:     true,
		GraphiQL:   false,
		Playground: true,
	})

	http.Handle("/graphql", h)
	log.Println("GraphQL Playground running on localhost:8080/graphql")

	port := 8888
	if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
		port = v
	}
	log.Printf("listen ... :%d", port)
	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)
}
