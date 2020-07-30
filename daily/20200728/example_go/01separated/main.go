package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"path"
	"strings"
)

func SplitPath(p string) (string, string) {
	np := path.Clean(p)
	i := strings.Index(np[1:], "/") + 1
	if i <= 1 {
		return np, ""
	}
	return np[1:i], np[i+1:]
}

func GetUserHandler(userID string) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprintf(w, `{"id": "%s"}`, userID)
	}
}
func NotFoundHandler(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(404)
	w.Header().Set("Content-Type", "application/json")
}

func UserHandler(path string) http.HandlerFunc {
	userID, right := SplitPath(path)
	if right != "" {
		return NotFoundHandler
	}
	return GetUserHandler(userID)
}

func NewHandler() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		head, tail := SplitPath(r.URL.Path)
		fmt.Printf("@%q,%q@\n", head, tail)

		switch head {
		case "users":
			UserHandler(tail)(w, r)
		default:
			http.NotFound(w, r)
		}
	})
}

func main() {
	h := NewHandler()
	log.Fatal(http.ListenAndServe(os.Getenv("ADDR"), h))
}
