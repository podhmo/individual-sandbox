package web

import (
	"errors"
	"fmt"
	"m/config"
	"m/store"
	"m/store/entity"
	"m/web/parser"
	"m/web/setup"
	"net/http"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/httplog"
	"github.com/go-chi/render"
	"github.com/rs/zerolog"
)

type Server struct {
	Router chi.Router

	Logger *zerolog.Logger

	Parser         *parser.Parser
	Store          *store.Store
	defaultHandler func(CustomHandler) http.HandlerFunc
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.Router.ServeHTTP(w, r)
}
func (s *Server) Get(path string, handler CustomHandler) {
	s.Router.Get(path, s.defaultHandler(handler))
}
func (s *Server) Post(path string, handler CustomHandler) {
	s.Router.Post(path, s.defaultHandler(handler))
}

func (s *Server) SendObject(w http.ResponseWriter, r *http.Request, ob interface{}) error {
	render.JSON(w, r, ob)
	return nil
}
func (s *Server) SendObjectWithStatus(w http.ResponseWriter, r *http.Request, ob interface{}, statusCode int) error {
	render.Status(r, statusCode)
	render.JSON(w, r, ob)
	return nil
}
func (s *Server) SendArray(w http.ResponseWriter, r *http.Request, items interface{}) error {
	if items == nil {
		items = []bool{} // zero length array
	}
	// TODO: https://opensource.zalando.com/restful-api-guidelines
	render.JSON(w, r, map[string]interface{}{"items": items})
	return nil
}

func NewServerFromConfig(c config.Config) *Server {
	return NewServer(&setup.Setup{Config: c})
}
func NewServer(setup *setup.Setup) *Server {
	setup.Finalize()
	store := setup.Store()
	parser := setup.Parser()
	logger := setup.Logger()

	var s *Server
	{
		// Router
		r := chi.NewRouter()
		// A good base middleware stack
		r.Use(middleware.RequestID)
		// r.Use(middleware.RealIP)

		// Logger (TODO: customize colorful output)
		r.Use(httplog.RequestLogger(*logger))
		// r.Use(middleware.Recoverer)

		s = &Server{Router: r, Parser: parser, Store: store, Logger: logger}
		s.defaultHandler = NewDefaultHandler(s)
	}

	s.Get("/api/todos", func(w http.ResponseWriter, r *http.Request) error {
		var items []*entity.Todo
		if err := store.Todo.List(r.Context(), &items); err != nil {
			return fmt.Errorf("store: %w", err)
		}
		return s.SendArray(w, r, items)
	})
	s.Post("/api/todos", func(w http.ResponseWriter, r *http.Request) error {
		var item entity.Todo
		if err := parser.Todo(r.Body, &item); err != nil {
			return fmt.Errorf("parse: %w", err)
		}
		if err := store.Todo.Add(r.Context(), &item); err != nil {
			return fmt.Errorf("store: %w", err)
		}
		return s.SendObjectWithStatus(w, r, &item, 201)
	})

	{
		// 500 example
		s.Get("/500", func(w http.ResponseWriter, r *http.Request) error {
			// return fmt.Errorf("action: %w", pkgerrors.WithStack(errors.New("hmm")))
			return fmt.Errorf("action: %w", errors.New("hmm"))
		})

		s.Get("/_panic", func(w http.ResponseWriter, r *http.Request) error {
			var n int
			func() {
				fmt.Println(1 / n)
			}()
			return nil
		})
	}

	{
		// TODO: method not allowed handler
		// default 404 handler
		s.Router.NotFound(func(w http.ResponseWriter, r *http.Request) {
			s.SendObjectWithStatus(w, r, map[string]interface{}{
				"method":  r.Method,
				"path":    r.URL.RequestURI(),
				"query":   r.URL.RawQuery,
				"code":    "not found",
				"message": fmt.Sprintf("%s %s is not found", r.Method, r.URL.RequestURI()),
			}, 404)
		})

		// TODO unauthorized
	}
	return s
}
