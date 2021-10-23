package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"reflect"
	"runtime"
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

type FuncInfo struct {
	Name string
}

type authRegister struct {
	factory             func(http.Handler) http.Handler
	needLoginHandlerPCs []uintptr
}

func (auth *authRegister) NeedLogin(h http.Handler) http.Handler {
	auth.needLoginHandlerPCs = append(auth.needLoginHandlerPCs, reflect.ValueOf(h).Pointer())
	return auth.factory(h)
}

func (auth *authRegister) Needed() []FuncInfo {
	r := make([]FuncInfo, len(auth.needLoginHandlerPCs))
	for i, pc := range auth.needLoginHandlerPCs {
		rfunc := runtime.FuncForPC(pc)
		r[i] = FuncInfo{Name: rfunc.Name()}
	}
	return r
}

func GetArticle(auth *authRegister, store *Store) http.Handler {
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
	// return WithMiddleware(h, LoginRequired(store))
	return auth.NeedLogin(h)
}

// type Middleware interface {
// 	Factory() func(http.Handler) http.Handler
// 	Unwrap() http.Handler
// 	http.Handler
// }

// type Adapter struct {
// 	factory    func(http.Handler) http.Handler
// 	underlying http.Handler
// 	http.Handler
// }

// func (a *Adapter) Factory() func(http.Handler) http.Handler {
// 	return a.factory
// }
// func (a *Adapter) Unwrap() http.Handler {
// 	return a.underlying
// }
// func WithMiddleware(h http.Handler, fs ...func(http.Handler) http.Handler) http.Handler {
// 	for _, f := range fs {
// 		h = &Adapter{
// 			Handler:    f(h),
// 			underlying: h,
// 		}
// 	}
// 	return h
// }

// func isLoginNeededHandler(h http.Handler) bool {
// 	target := h
// 	for {
// 		if a, ok := h.(interface {
// 			Factory() func(http.Handler) http.Handler
// 		}); ok {
// 			// FIXME: always false (because a.Factory() is returned closure)
// 			if reflect.DeepEqual(a.Factory(), LoginRequired) {
// 				return true
// 			}
// 		}

// 		switch h := target.(type) {
// 		case interface{ Unwrap() http.Handler }:
// 			target = h.Unwrap()
// 		default:
// 			return false
// 		}
// 	}
// }

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
	auth := &authRegister{factory: LoginRequired(store)}
	handler := GetArticle(auth, store)

	fmt.Println(auth.Needed()) // hmm

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
