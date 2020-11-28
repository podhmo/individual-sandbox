package main

import (
	"fmt"
	"log"
	"m/00hand/action"
	"m/00hand/handler"
	"net/http"
	"os"

	"github.com/podhmo/tenuki"
)

func main() {
	addr := ":33333"
	if v := os.Getenv("ADDR"); v != "" {
		addr = v
	}

	if err := Run(addr); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Run(addr string) error {
	mux := http.DefaultServeMux
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		tenuki.Render(w, r).JSON(200, map[string]interface{}{
			"methods": []string{"Hello"},
		})
	})
	mux.HandleFunc("/Hello", WebAPIAdapter(handler.Hello))
	mux.HandleFunc("/IsEven", WebAPIAdapter(handler.IsEven))
	mux.HandleFunc("/AddTodo", WebAPIAdapter(handler.AddTodo))
	mux.HandleFunc("/ListTodo", WebAPIAdapter(handler.ListTodo))
	mux.HandleFunc("/Greet", WebAPIAdapter(handler.Greet))

	log.Printf("listen %s...", addr)
	return http.ListenAndServe(addr, mux)
}

// ---

func WebAPIAdapter(h handler.HandlerFunc) http.HandlerFunc {
	name := fmt.Sprintf("%s", h) // todo: fix
	return func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			t := recover()
			if t != nil {
				log.Printf("hmm %+v", t)
				tenuki.Render(w, r).JSON(500, map[string]string{"message": fmt.Sprintf("%s", t)})
			}
		}()

		// TODO:headers and queries
		ev := handler.Event{
			Name:    name,
			Body:    r.Body,
			Headers: r.URL.Query(), // + headers
		}

		ctx := handler.WithEvent(action.SetupContext(r.Context()), ev)
		result, err := h(ctx, ev)
		if err != nil {
			code := 500
			if x, ok := err.(interface{ StatusCode() int }); ok {
				code = x.StatusCode()
			}
			// logはどうしよう
			tenuki.Render(w, r).JSON(code, map[string]string{"message": err.Error()})
			return
		}
		tenuki.Render(w, r).JSON(200, result)
	}
}
