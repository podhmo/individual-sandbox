package main

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strconv"
	"testing"

	"github.com/go-chi/chi/v5"
	"github.com/go-chi/render"
	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

type Todo struct {
	ID    int    `json:"id"`
	Title string `json:"title"`
}

var todos = []Todo{
	{ID: 1, Title: "foo"},
	{ID: 2, Title: "bar"},
	{ID: 3, Title: "boo"},
}

func GetTodo(w http.ResponseWriter, req *http.Request) {
	todoID, err := strconv.Atoi(chi.URLParam(req, "todoId"))
	if err != nil {
		render.Status(req, 404)
		render.JSON(w, req, map[string]string{"error": http.StatusText(404)})
	}

	for _, x := range todos {
		if x.ID == todoID {
			render.JSON(w, req, x)
			return
		}
	}
	render.Status(req, 404)
	render.JSON(w, req, map[string]string{"error": http.StatusText(404)})
}

func NewRouter() chi.Router {
	r := chi.NewRouter()
	r.Get("/todos/{todoId}", GetTodo)
	return r
}

func TestGetTodo(t *testing.T) {
	router := NewRouter()

	path := "/todos/1"
	req := httptest.NewRequest("GET", path, nil)
	rec := httptest.NewRecorder()
	router.ServeHTTP(rec, req)
	res := rec.Result()

	if want, got := 200, res.StatusCode; want != got {
		t.Errorf("%q, status: want=%d != got=%d", path, want, got)
	}

	var got Todo
	if err := json.NewDecoder(res.Body).Decode(&got); err != nil {
		t.Errorf("%q, unexpected error: %+v", path, err)
	}

	want := Todo{Title: "foo"}
	if diff := cmp.Diff(want, got, cmpopts.IgnoreFields(Todo{}, "ID")); diff != "" {
		t.Errorf("%q type %T mismatch (-want +got):\n%s", path, want, diff)
	}
}
