package web

import (
	"fmt"
	"m/config"
	"m/store/entity"
	"m/web/setup"
	"net/http"

	"github.com/go-chi/chi"
	"github.com/go-chi/httplog"
	"github.com/rs/zerolog"
)

type Server struct {
	Router chi.Router
	Logger *zerolog.Logger

	setup *setup.Setup

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
func (s *Server) Patch(path string, handler CustomHandler) {
	s.Router.Patch(path, s.defaultHandler(handler))
}

func NewServerFromConfig(c config.Config) *Server {
	return NewServer(&setup.Setup{Config: c})
}
func NewServer(setup *setup.Setup) *Server {
	setup.Finalize()
	parser := setup.Parser()
	_ = parser
	logger := setup.Logger()

	var s *Server
	{
		// Router
		r := chi.NewRouter()

		// r.Use(middleware.RealIP)
		// Logger (TODO: customize colorful output)
		r.Use(httplog.RequestLogger(*logger))
		// r.Use(middleware.Recoverer)

		s = &Server{Router: r, setup: setup, Logger: logger}
		s.defaultHandler = NewDefaultHandler(s)
	}

	s.Get("/api/todos", func(as AppSession, w http.ResponseWriter, r *http.Request) error {
		var items []entity.Todo

		u := s.ResolveTodo(as)
		if err := u.ListTodo(r.Context(), &items); err != nil {
			return fmt.Errorf("query: %w", err)
		}
		return s.SendArray(w, r, items, len(items))
	})

	s.Post("/api/todos", func(as AppSession, w http.ResponseWriter, r *http.Request) error {
		var item entity.Todo

		u := s.ResolveTodo(as)
		if err := parser.Todo(r.Body, &item); err != nil {
			return fmt.Errorf("parse: %w", err)
		}
		if err := u.AddTodo(r.Context(), item); err != nil {
			return fmt.Errorf("command: %w", err)
		}
		return s.SendObjectWithStatus(w, r, &item, 201)
	})

	s.Patch("/api/todos/{no}", func(as AppSession, w http.ResponseWriter, r *http.Request) error {
		var items []entity.Todo
		var no int64

		u := s.ResolveTodo(as)
		if err := parser.Int64(chi.URLParam(r, "no"), &no); err != nil {
			return err
		}

		if err := u.DoneTodo(r.Context(), &items, int(no)); err != nil {
			return fmt.Errorf("command: %w", err)
		}
		return s.SendArray(w, r, &items, len(items))
	})

	// {
	// 	// 500 example
	// 	s.Get("/500", func(as AppSession, w http.ResponseWriter, r *http.Request) error {
	// 		// return fmt.Errorf("action: %w", pkgerrors.WithStack(errors.New("hmm")))
	// 		return fmt.Errorf("action: %w", errors.New("hmm"))
	// 	})

	// 	s.Get("/_panic", func(as AppSession, w http.ResponseWriter, r *http.Request) error {
	// 		var n int
	// 		func() {
	// 			fmt.Println(1 / n)
	// 		}()
	// 		return nil
	// 	})
	// }

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
