package main

import (
	"log"
	"net/http"
	"os"

	"github.com/podhmo/tenuki"
)

type Todo struct {
	Title string `json:"title"`
}

func ListTodo() ([]Todo, error) {
	return []Todo{{Title: "hello"}}, nil
}

type Router interface {
	Register(
		method, path string, interactor interface{},
		handler http.HandlerFunc,
	)
}

type muxRouter struct {
	*http.ServeMux
}

func (r *muxRouter) Register(
	method, path string, interactor interface{},
	handler http.HandlerFunc,
) {
	r.HandleFunc(path, handler)
}

func main() {
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":44444"
	}

	mux := &http.ServeMux{}
	r := &muxRouter{ServeMux: mux}

	// 考えてみると、これはこういう話か。
	// @route("GET", path="/hello")
	// def ListTodo():
	//     pass
	// 何かでwrappingしたいかどうかという話？

	r.Register(
		"GET", "/todo", ListTodo,
		func(w http.ResponseWriter, r *http.Request) {
			items, _ := ListTodo()
			tenuki.Render(w, r).JSONArray(200, items)
		},
	)

	log.Println("listening", addr)
	if err := http.ListenAndServe(addr, mux); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
