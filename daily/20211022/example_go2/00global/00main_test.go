package main

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

type Store struct {
	articles map[string]*Article
}

type Article struct {
	Title string `json:"title"`
}

func (s *Store) FindArticleByID(id string) *Article {
	return s.articles[id]
}

var store = &Store{
	articles: map[string]*Article{
		"1": {Title: "hello"},
	},
}

func Handler(w http.ResponseWriter, r *http.Request) {
	parts := strings.Split(r.URL.Path, "/")
	articleID := parts[len(parts)-1]

	ob := store.FindArticleByID(articleID)
	if ob == nil {
		w.WriteHeader(http.StatusNotFound)
		return
	}
	b, err := json.Marshal(ob)
	if err != nil {
		http.Error(w, "ISE", http.StatusInternalServerError)
		return
	}
	w.Write(b)
}

func Test(t *testing.T) {
	rec := httptest.NewRecorder()
	Handler(rec, httptest.NewRequest("GET", "/articles/1", nil))

	if want, got := http.StatusOK, rec.Code; want != got {
		t.Errorf("want status\n\t%d\nbut got\n\t%d", want, got)
	}
}
