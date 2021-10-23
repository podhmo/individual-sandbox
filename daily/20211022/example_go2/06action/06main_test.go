package main

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/go-chi/chi/v5"
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

type GetArticleHandler struct {
	Store *Store
}

func (h *GetArticleHandler) Do(articleID string) *Article {
	return h.Store.FindArticleByID(articleID)
}

func (h *GetArticleHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	parts := strings.Split(r.URL.Path, "/")
	articleID := parts[len(parts)-1]

	ob := h.Do(articleID)
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

func NewGetArticleHandler(store *Store) *GetArticleHandler {
	return &GetArticleHandler{Store: store}
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

func NewRouter(store *Store) chi.Router {
	r := chi.NewRouter()
	{
		h := NewGetArticleHandler(store)
		// fmt.Println(reflect.TypeOf(h.Do)) // func(string) *main.Article
		r.With(LoginRequired(store)).Get("/articles/{articleId}", h.ServeHTTP)
	}
	return r
}

func Test(t *testing.T) {
	store := &Store{
		articles: map[string]*Article{
			"1": {Title: "hello"},
		},
		users: map[string]*User{"me": {}},
	}

	r := NewRouter(store)
	ts := httptest.NewServer(r)
	defer ts.Close()

	t.Run("401", func(t *testing.T) {
		res, err := http.Get(ts.URL + "/articles/1")
		if err != nil {
			t.Fatalf("unexpected %+v", err)
		}
		if want, got := http.StatusUnauthorized, res.StatusCode; want != got {
			t.Errorf("want status\n\t%d\nbut got\n\t%d", want, got)
		}
	})

	t.Run("200", func(t *testing.T) {
		req, err := http.NewRequest("GET", ts.URL+"/articles/1", nil)
		if err != nil {
			t.Fatalf("unexpected %+v", err)
		}
		req.Header.Set("Authorization", "Bearer me")
		res, err := http.DefaultClient.Do(req)
		if err != nil {
			t.Fatalf("unexpected %+v", err)
		}
		if want, got := http.StatusOK, res.StatusCode; want != got {
			t.Errorf("want status\n\t%d\nbut got\n\t%d", want, got)
		}
	})
}
