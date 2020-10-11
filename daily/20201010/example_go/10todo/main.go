package main

import (
	"context"
	"encoding/json"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"reflect"
	"strconv"
	"time"

	"github.com/getkin/kin-openapi/openapi3"
	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
	reflectopenapi "github.com/podhmo/reflect-openapi"
)

type Store struct {
	Todos []Todo
	Now   func() time.Time
}

type Todo struct {
	Title     string    `json:"title"`
	CreatedAt time.Time `json:"createdAt" required:"false"`
	Done      bool      `json:"done" required:"false"`
}

func (s *Store) ListTodo() []Todo {
	return s.Todos
}

var ztime time.Time

func (s *Store) AddTodo(todo Todo) Todo {
	if todo.CreatedAt == ztime {
		todo.CreatedAt = s.Now()
	}
	s.Todos = append(s.Todos, todo)
	return todo
}

type APIError struct {
	Code    int    `json:"-"`
	Message string `json:"message"`
}

func (e *APIError) Error() string {
	return e.Message
}

func ParseJSON(r io.Reader, ob interface{}) error {
	decoder := json.NewDecoder(r)
	if err := decoder.Decode(ob); err != nil {
		return &APIError{Message: err.Error(), Code: 400}
	}
	defer io.Copy(ioutil.Discard, r)
	return nil
}

type CustomHandler func(http.ResponseWriter, *http.Request) error

type Server struct {
	Store *Store
}

func (s *Server) ListTodo(w http.ResponseWriter, r *http.Request) error {
	retval := s.Store.ListTodo()
	render.JSON(w, r, retval)
	return nil
}
func (s *Server) AddTodo(w http.ResponseWriter, r *http.Request) error {
	var ob Todo
	if err := ParseJSON(r.Body, &ob); err != nil {
		return err
	}
	retval := s.Store.AddTodo(ob)
	render.Status(r, 201)
	render.JSON(w, r, retval)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	s := Server{
		Store: &Store{
			Todos: []Todo{
				{Title: "sleeping", CreatedAt: time.Now()},
			},
			Now: time.Now,
		},
	}

	r := chi.NewRouter()
	r.Use(middleware.Logger)

	c := reflectopenapi.Config{
		StrictSchema:   true,
		SkipValidation: true,
		DefaultError:   APIError{},
		IsRequiredCheckFunction: func(tag reflect.StructTag) bool {
			v, exists := tag.Lookup("required")
			if !exists {
				return true // required
			}
			required, err := strconv.ParseBool(v)
			if err != nil {
				return false // unrequired
			}
			return required
		},
	}
	doc, err := c.BuildDoc(context.Background(), func(m *reflectopenapi.Manager) {
		lift := func(ch CustomHandler) http.HandlerFunc {
			return func(w http.ResponseWriter, req *http.Request) {
				err := ch(w, req)
				if err == nil {
					return
				}

				if apiErr, ok := err.(*APIError); ok {
					render.Status(req, apiErr.Code)
					render.JSON(w, req, apiErr)
					return
				}

				render.Status(req, 500)
				render.JSON(w, req, &APIError{
					Code: 500, Message: err.Error(),
				})
			}
		}

		type modifyOption func(*openapi3.Operation)
		addEndpoint := func(method, path string,
			interactor interface{},
			handler CustomHandler,
			modifiers ...modifyOption,
		) {
			r.Method(method, path, lift(handler))
			op := m.Visitor.VisitFunc(interactor)
			for _, modify := range modifiers {
				modify(op)
			}
			m.Doc.AddOperation(path, method, op)
		}

		withDefaultCode := func(code int) modifyOption {
			return func(op *openapi3.Operation) {
				op.Responses[strconv.Itoa(code)] = op.Responses["200"]
				delete(op.Responses, "200")
			}
		}

		addEndpoint("GET", "/todos",
			s.Store.ListTodo,
			s.ListTodo,
		)
		addEndpoint("POST", "/todos",
			s.Store.AddTodo,
			s.AddTodo,
			withDefaultCode(201),
		)
	})
	if err != nil {
		return err
	}

	if ok, _ := strconv.ParseBool(os.Getenv("GENDOC")); ok {
		log.Println("generate openapi doc")
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		return enc.Encode(doc)
	}

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":8888"
	}
	log.Println("listening", addr)
	return http.ListenAndServe(addr, r)
}
