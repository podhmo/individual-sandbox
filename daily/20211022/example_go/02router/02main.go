package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/go-chi/chi/v5"
	"github.com/go-chi/chi/v5/middleware"
	"github.com/go-chi/render"
)

type Article struct {
	Title string `json:"title"`
}

var articles = map[string]*Article{
	"1": {Title: "hello"},
	"2": {Title: "byebye"},
}

func ListArticle(w http.ResponseWriter, r *http.Request) {
	result := make([]*Article, 0, len(articles))
	for _, ob := range articles {
		result = append(result, ob)
	}
	render.JSON(w, r, result)
}

func GetArticle(w http.ResponseWriter, r *http.Request) {
	articleID := chi.URLParam(r, "articleId")
	ob, ok := articles[articleID]
	if !ok {
		// go-chi/renderを使わない場合には w.WriteHeader()を使う
		r := r.WithContext(context.WithValue(r.Context(), render.StatusCtxKey, http.StatusNotFound))
		render.JSON(w, r, map[string]string{"message": "not found"})
		return
	}
	render.JSON(w, r, ob)
}

func main() {
	port := 8888
	if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
		port = v
	}
	log.Print("listen ...", port)

	r := chi.NewRouter()
	r.Use(middleware.Logger)

	r.Get("/articles", ListArticle)
	r.Get("/articles/{articleId}", GetArticle)

	r.NotFound(func(w http.ResponseWriter, r *http.Request) {
		r = r.WithContext(context.WithValue(r.Context(), render.StatusCtxKey, http.StatusNotFound))
		render.JSON(w, r, map[string]string{"message": http.StatusText(http.StatusNotFound)})
	})

	addr := fmt.Sprintf(":%d", port)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
