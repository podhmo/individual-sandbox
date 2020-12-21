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

func Hello(name string) string {
	return fmt.Sprintf("Hello %s", name)
}

func setupHandler(addr string) http.Handler {
	mux := &http.ServeMux{}

	c := &reflectopenapi.Config{
		Selector: &struct {
			reflectopenapi.MergeParamsInputSelector
			reflectopenapi.FirstParamOutputSelector
		}{},
	}
	c.BuildDoc(context.Background(), func(m *reflectopenapi.Manager) {
		{
			path := "/hello"
			mux.HandleFunc(path, func(w http.ResponseWriter, r *http.Request) {
				var input struct {
					Name string `json:"name"`
				}
				if err := json.NewDecoder(r.Body).Decode(&input); err != nil {
					fmt.Fprintf(w, `{"error": %q}`, err.Error())
					return
				}
				defer r.Body.Close()
				fmt.Fprintf(w, `%q`, Hello(input.Name))
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
