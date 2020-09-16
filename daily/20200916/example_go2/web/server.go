package web

import (
	"encoding/json"
	"fmt"
	"m/config"
	"m/store/entity"
	"m/web/setup"
	"net/http"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/httplog"
	"github.com/go-chi/render"
)

type Server struct {
	chi.Router
}

func NewServerFromConfig(c config.Config) *Server {
	return NewServer(&setup.Setup{Config: c})
}
func NewServer(s *setup.Setup) *Server {
	s.Finalize()
	store := s.Store()

	// Router
	r := chi.NewRouter()

	// A good base middleware stack
	r.Use(middleware.RequestID)
	// r.Use(middleware.RealIP)

	// Logger (TODO: customize colorful output)
	r.Use(httplog.RequestLogger(*s.Logger()))

	r.Use(middleware.Heartbeat("/ping"))
	r.Use(middleware.Recoverer)

	r.Get("/api/todos", func(w http.ResponseWriter, r *http.Request) {
		var items []*entity.Todo
		if err := store.Todo.List(r.Context(), &items); err != nil {
			render.Status(r, 500)
			render.JSON(w, r, map[string]interface{}{
				"message": err,
			})
			return
		}
		if items == nil {
			items = []*entity.Todo{}
		}
		// TODO: https://opensource.zalando.com/restful-api-guidelines
		render.JSON(w, r, map[string]interface{}{"items": items})
	})
	r.Post("/api/todos", func(w http.ResponseWriter, r *http.Request) {
		decoder := json.NewDecoder(r.Body)
		var item entity.Todo
		if err := decoder.Decode(&item); err != nil {
			render.Status(r, 400)
			render.JSON(w, r, err)
			return
		}
		defer r.Body.Close()
		if err := store.Todo.Add(r.Context(), &item); err != nil {
			render.Status(r, 500)
			render.JSON(w, r, map[string]interface{}{
				"message": err,
			})
			return
		}
		render.Status(r, 201)
		render.JSON(w, r, &item)
	})

	r.Get("/_panic", func(w http.ResponseWriter, r *http.Request) {
		var n int
		func() {
			fmt.Println(1 / n)
		}()
	})

	// default 404 handler
	r.NotFound(func(w http.ResponseWriter, r *http.Request) {
		render.Status(r, 404)
		render.JSON(w, r, map[string]interface{}{
			"method":  r.Method,
			"path":    r.URL.RequestURI(),
			"query":   r.URL.RawQuery,
			"code":    "not found",
			"message": fmt.Sprintf("%s %s is not found", r.Method, r.URL.RequestURI()),
		})
	})

	// TODO unauthorized
	return &Server{Router: r}
}
