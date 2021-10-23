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
	users    map[string]*User
}

type Article struct {
	Title string `json:"title"`
}

type User struct{}

func (s *Store) FindArticleByID(id string) *Article {
	return s.articles[id]
}

func GetArticle(store *Store) http.Handler {
	var h http.HandlerFunc = func(w http.ResponseWriter, r *http.Request) {
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
	return LoginRequired(store)(h)
}

func LoginRequired(store *Store) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			k := strings.TrimPrefix(r.Header.Get("Authorization"), "Bearer ")
			_, ok := store.users[k]
			if !ok {
				w.Header().Set("WWW-Authenticate", `Bearer realm="token_required"`)
				w.WriteHeader(http.StatusUnauthorized)
				return
			}
			// TODO: bind loginUser
			next.ServeHTTP(w, r)
		})
	}
}

func Test(t *testing.T) {
	store := &Store{
		articles: map[string]*Article{
			"1": {Title: "hello"},
		},
		users: map[string]*User{"me": {}},
	}
	handler := GetArticle(store)

	t.Run("401", func(t *testing.T) {
		rec := httptest.NewRecorder()
		handler.ServeHTTP(rec, httptest.NewRequest("GET", "/articles/1", nil))

		if want, got := http.StatusUnauthorized, rec.Code; want != got {
			t.Errorf("want status\n\t%d\nbut got\n\t%d", want, got)
		}
	})

	t.Run("200", func(t *testing.T) {
		rec := httptest.NewRecorder()

		req := httptest.NewRequest("GET", "/articles/1", nil)
		req.Header.Set("Authorization", "Bearer me")
		handler.ServeHTTP(rec, req)

		if want, got := http.StatusOK, rec.Code; want != got {
			t.Errorf("want status\n\t%d\nbut got\n\t%d", want, got)
		}
	})
}
