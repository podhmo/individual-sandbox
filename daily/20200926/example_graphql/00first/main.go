package main

// https://github.com/zupzup/example-go-graphql

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	"github.com/graphql-go/graphql"
	gqlhandler "github.com/graphql-go/graphql-go-handler"
)

// Post is a post
type Post struct {
	UserID int    `json:"userId"`
	ID     int    `json:"id"`
	Title  string `json:"title"`
	Body   string `json:"body"`
}

// Comment is a comment
type Comment struct {
	PostID int    `json:"postId"`
	ID     int    `json:"id"`
	Name   string `json:"name"`
	Email  string `json:"email"`
	Body   string `json:"body"`
}

func NewHandler() (http.Handler, error) {
	schema, err := graphql.NewSchema(graphql.SchemaConfig{
		Query: graphql.NewObject(
			createQueryType(
				createPostType(
					createCommentType(),
				),
			),
		),
	})
	if err != nil {
		return nil, err
	}

	mux := &http.ServeMux{}
	mux.Handle("/graphql", gqlhandler.New(&gqlhandler.Config{
		Schema: &schema,
	}))
	return mux, nil
}

func main() {
	mux, err := NewHandler()
	if err != nil {
		log.Fatalf("failed to create new schema, error: %v", err)
	}

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":4444"
	}
	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, mux); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func createQueryType(postType *graphql.Object) graphql.ObjectConfig {
	return graphql.ObjectConfig{Name: "QueryType", Fields: graphql.Fields{
		"post": &graphql.Field{
			Type: postType,
			Args: graphql.FieldConfigArgument{
				"id": &graphql.ArgumentConfig{
					Type: graphql.NewNonNull(graphql.Int),
				},
			},
			Resolve: func(p graphql.ResolveParams) (interface{}, error) {
				id := p.Args["id"]
				v, _ := id.(int)
				log.Printf("fetching post with id: %d", v)
				return fetchPostByiD(v)
			},
		},
	}}
}

func createPostType(commentType *graphql.Object) *graphql.Object {
	return graphql.NewObject(graphql.ObjectConfig{
		Name: "Post",
		Fields: graphql.Fields{
			"userId": &graphql.Field{
				Type: graphql.NewNonNull(graphql.Int),
			},
			"id": &graphql.Field{
				Type: graphql.NewNonNull(graphql.Int),
			},
			"title": &graphql.Field{
				Type: graphql.String,
			},
			"body": &graphql.Field{
				Type: graphql.String,
			},
			"comments": &graphql.Field{
				Type: graphql.NewList(commentType),
				Resolve: func(p graphql.ResolveParams) (interface{}, error) {
					post, _ := p.Source.(*Post)
					log.Printf("fetching comments of post with id: %d", post.ID)
					return fetchCommentsByPostID(post.ID)
				},
			},
		},
	})
}

func createCommentType() *graphql.Object {
	return graphql.NewObject(graphql.ObjectConfig{
		Name: "Comment",
		Fields: graphql.Fields{
			"postId": &graphql.Field{
				Type: graphql.NewNonNull(graphql.Int),
			},
			"id": &graphql.Field{
				Type: graphql.NewNonNull(graphql.Int),
			},
			"name": &graphql.Field{
				Type: graphql.String,
			},
			"email": &graphql.Field{
				Type: graphql.String,
			},
			"body": &graphql.Field{
				Type: graphql.String,
			},
		},
	})
}

func fetchPostByiD(id int) (*Post, error) {
	resp, err := http.Get(fmt.Sprintf("http://jsonplaceholder.typicode.com/posts/%d", id))
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("%s: %s", "could not fetch data", resp.Status)
	}
	b, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, errors.New("could not read data")
	}
	result := Post{}
	err = json.Unmarshal(b, &result)
	if err != nil {
		return nil, errors.New("could not unmarshal data")
	}
	return &result, nil
}

func fetchCommentsByPostID(id int) ([]Comment, error) {
	resp, err := http.Get(fmt.Sprintf("http://jsonplaceholder.typicode.com/posts/%d/comments", id))
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return nil, fmt.Errorf("%s: %s", "could not fetch data", resp.Status)
	}
	b, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, errors.New("could not read data")
	}
	result := []Comment{}
	err = json.Unmarshal(b, &result)
	if err != nil {
		return nil, errors.New("could not unmarshal data")
	}
	return result, nil
}
