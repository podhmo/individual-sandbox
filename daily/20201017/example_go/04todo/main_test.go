package main

import (
	"net/http"
	"net/http/httptest"
	"reflect"
	"testing"

	"github.com/podhmo/tenuki"
)

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

func ListTodo(w http.ResponseWriter, req *http.Request) {
	tenuki.Render(w, req).JSONArray(200, []Todo{{
		Title: "hello",
	}})
}

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(ListTodo))
	defer ts.Close()

	f := tenuki.New(t)
	req := f.NewRequest("GET", ts.URL+"/todo", nil)
	res := f.Do(req)

	if want, got := 200, res.StatusCode; want != got {
		t.Errorf("status code\nwant\n\t%d\nbut\n\t%d", want, got)
	}

	want := []Todo{Todo{Title: "hello"}}
	var got []Todo
	f.Extract().JSON(res, &got)
	if !reflect.DeepEqual(want, got) {
		t.Errorf("response body\nwant\n\t%+v\nbut\n\t%+v", want, got)
	}
}
