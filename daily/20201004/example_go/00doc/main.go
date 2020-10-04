package main

import (
	"context"
	"encoding/json"
	"log"
	"os"
	"reflect"
	"strconv"

	reflectopenapi "github.com/podhmo/reflect-openapi"
)

type User struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
	Age  string `json:"age"`
}

type ListUserInput struct {
	Limit int `json:"limit" openapi:"query"`
}
type InsertUserInput struct {
	User
}
type PutUserInput struct {
	User
	UserID int `json:"userId" openapi:"path"`
}
type FindUserInput struct {
	UserID int `json:"userId" openapi:"path"`
}

func ListUser(input ListUserInput) ([]User, error) {
	return nil, nil
}
func FindUser(input FindUserInput) (User, error) {
	return User{}, nil
}
func InsertUser(input InsertUserInput) (User, error) {
	return User{}, nil
}
func PutUser(input PutUserInput) (User, error) {
	return User{}, nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	c := reflectopenapi.Config{
		StrictSchema: true,
		IsRequiredCheckFunction: func(tag reflect.StructTag) bool {
			val, exists := tag.Lookup("required")
			if !exists {
				return true
			}
			required, _ := strconv.ParseBool(val)
			return required
		},
	}

	doc, err := c.BuildDoc(context.Background(), func(m *reflectopenapi.Manager) {
		// 通常はコレと一緒にhandlerも受け取る。普通はstructを定義してmethodにする
		addEndpoint := func(method, path string, interactor interface{}) {
			op := m.Visitor.VisitFunc(interactor)
			m.Doc.AddOperation(path, method, op)
		}

		addEndpoint("GET", "/users", ListUser)
		addEndpoint("POST", "/users", InsertUser)
		addEndpoint("GET", "/users/{userId}", FindUser)
		addEndpoint("PUT", "/users/{userId}", PutUser)
	})
	if err != nil {
		return err
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(doc)
}
