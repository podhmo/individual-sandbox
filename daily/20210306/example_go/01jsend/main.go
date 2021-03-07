package main

import (
	"fmt"
	"m/jsend"
	"net/http"

	"github.com/podhmo/tenuki"
)

// https://github.com/omniti-labs/jsend

type Post struct {
	ID    int64  `json:"id"`
	Title string `json:"title"`
	Body  string `json:"body"`
}

const (
	MessageBadRequest jsend.Message = "bad request"
)

func SuccessHandler(w http.ResponseWriter, r *http.Request) {
	tenuki.Render(w, r).JSON(200, jsend.Success(map[string]interface{}{
		"posts": []Post{
			Post{ID: 1, Title: "A blog post", Body: "Some useful content"},
			Post{ID: 2, Title: "Another blog post", Body: "More content"},
		},
	}))
}

func FailHandler(w http.ResponseWriter, r *http.Request) {
	tenuki.Render(w, r).JSON(400, jsend.Fail(map[string]interface{}{
		"title": "A title is required",
	}, MessageBadRequest))
}

func ErrorHandler(w http.ResponseWriter, r *http.Request) {
	err := fmt.Errorf("Unable to communicate with database")
	// TODO: stack trace if debug
	tenuki.Render(w, r).JSON(500, jsend.Error(jsend.Message(err.Error())))
}

func main() {}
