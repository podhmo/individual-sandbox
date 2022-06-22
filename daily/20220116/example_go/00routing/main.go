package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

type User struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

var (
	users = []User{
		{ID: 1, Name: "foo"},
		{ID: 2, Name: "bar"},
	}
)

type APIError struct {
	Message string `json:"message"`
	status int
}

func (e APIError) Status() int {
	return e.status
}
func (e APIError) Error() string {
	return fmt.Sprintf("APIError: %s", e.Message)
}
func NewAPIError(status int, message string) APIError {
	return APIError{Message: message, status: status}
}


// ListUsers returns a list of users.
func ListUsers(w http.ResponseWriter, req *http.Request) ([]User, error) {
	return users, nil
}

// GetUser returns user
func GetUser(w http.ResponseWriter, req *http.Request) (User, error){
	var input GetUserInput
	if err := input.Bind(req); err != nil {
		return User{}, NewAPIError(400, err.Error())
	}
	user, err := getUser(input)
	if err != nil {
		return User{}, NewAPIError(404, err.Error())
	}
	return user, err
}

type GetUserInput struct {
	UserID int `json:"userId" openapi:"path"`
}

func (input *GetUserInput) Bind(req *http.Request) error {
	v := chi.URLParam(req, "userId")
	userID, err := strconv.ParseInt(v, 10, 0)
	if err != nil {
		return err
	}
	input.UserID = int(userID)
	return nil
}

func getUser(input GetUserInput) (User, error) {
	userID := input.UserID
	for _, u := range users {
		if u.ID == userID {
			return u, nil
		}
	}
	return User{}, fmt.Errorf("not found")
}

func lift[T any](action func(http.ResponseWriter, *http.Request) (T, error)) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		result, err := action(w, req)
		if err != nil {
			status := 500
			if t, ok := err.(interface { Status() int } ); ok {
				status = t.Status()
			}
			render.Status(req, status)
			render.JSON(w, req, err)
			return
		}
		render.JSON(w, req, result)
	}
}


func Mount(r chi.Router) {
	r.Get("/users", lift(ListUsers))
	r.Get("/users/{userId}", lift(GetUser))
}

// ----------------------------------------

func main() {
	useDocF := flag.Bool("doc", false, "generate doc")
	flag.Parse()

	if err := run(*useDocF); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(useDoc bool) error {
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":44444"
	}

	r := chi.NewRouter()
	r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	Mount(r)

	log.Println("listening ...", addr)
	return http.ListenAndServe(addr, r)
}
