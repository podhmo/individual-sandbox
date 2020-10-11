package main

import (
	"context"
	"encoding/json"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	reflectopenapi "github.com/podhmo/reflect-openapi"
)

type Setup struct {
	Router  chi.Router
	Manager *reflectopenapi.Manager
}

func (s *Setup) AddEndpoint(method, path string, interactor interface{}, handle http.HandlerFunc) {
	s.Router.Method(method, path, handle)
	op := s.Manager.Visitor.VisitFunc(interactor)
	s.Manager.Doc.AddOperation(path, method, op)
}

func (s *Setup) SetupRoutes() {
	s.AddEndpoint(
		"GET", "/",
		func() string { return "" }, // この関数からpathを導出
		func(w http.ResponseWriter, r *http.Request) {
			w.Write([]byte("welcome"))
		},
	)
}

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)
	c := reflectopenapi.Config{}

	doc, err := c.BuildDoc(context.Background(), func(m *reflectopenapi.Manager) {
		s := &Setup{Manager: m, Router: r}
		s.SetupRoutes()
	})
	if err != nil {
		log.Fatal(err)
	}
	if ok, _ := strconv.ParseBool(os.Getenv("DOCGEN")); ok {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		enc.Encode(doc)
		return
	}

	http.ListenAndServe(":3000", r)
}
