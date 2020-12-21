package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/getkin/kin-openapi/openapi3"
	reflectopenapi "github.com/podhmo/reflect-openapi"
	"github.com/podhmo/reflect-openapi/handler"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	addr := ":44444"
	if v := os.Getenv("ADDR"); v != "" {
		addr = v
	}
	h := setupHandler(addr)
	log.Println("Listen ...", addr)
	return http.ListenAndServe(addr, h)
}

type HelloInput struct{ Name string }

func Hello(input HelloInput) string {
	return fmt.Sprintf("Hello %s", input.Name)
}

func setupHandler(addr string) http.Handler {
	mux := &http.ServeMux{}

	c := &reflectopenapi.Config{}
	c.BuildDoc(context.Background(), func(m *reflectopenapi.Manager) {
		{
			path := "/hello"
			mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
				var input HelloInput
				if err := json.NewDecoder(r.Body).Decode(&input); err != nil {
					fmt.Fprintf(w, `{"error": %q}`, err.Error())
					return
				}
				defer r.Body.Close()
				fmt.Fprintf(w, `%q`, Hello(input))
			})

			op := m.Visitor.VisitFunc(Hello)
			m.Doc.AddOperation(path, "POST", op)
		}

		// swagger-ui
		doc := m.Doc
		doc.Servers = append([]*openapi3.Server{{
			URL:         fmt.Sprintf("http://localhost%s", addr),
			Description: "local development server",
		}}, doc.Servers...)
		mux.Handle("/openapi/", handler.NewHandler(doc, "/openapi/"))
	})
	return mux
}
