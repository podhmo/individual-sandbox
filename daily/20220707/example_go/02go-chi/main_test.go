package main

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strconv"
	"strings"
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

func DecodeJSONResponse[T any](t *testing.T, path string, res *http.Response, code int) T {
	t.Helper()

	defer res.Body.Close()
	var got T
	if wantCode, gotCode := code, res.StatusCode; wantCode != gotCode {
		buf := new(strings.Builder)
		io.Copy(buf, res.Body)
		defer t.Logf("\tresponse: %s", buf.String())
		t.Fatalf("%q, status: want=%d != got=%d", path, wantCode, gotCode)
	}

	if err := json.NewDecoder(res.Body).Decode(&got); err != nil {
		t.Errorf("%q, unexpected error (decode %T): %+v", path, got, err)
	}
	return got
}

func TestGetTodo(t *testing.T) {
	router := NewRouter()

	path := "/todos/4"
	req := httptest.NewRequest("GET", path, nil)
	rec := httptest.NewRecorder()
	router.ServeHTTP(rec, req)
	res := rec.Result()

	got := DecodeJSONResponse[Todo](t, path, res, 200)
	want := Todo{Title: "foo"}

	if diff := cmp.Diff(want, got, cmpopts.IgnoreFields(Todo{}, "ID")); diff != "" {
		t.Errorf("%q type %T mismatch (-want +got):\n%s", path, want, diff)
	}
}
