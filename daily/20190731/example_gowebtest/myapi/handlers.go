package myapi

import (
	"fmt"
	"net/http"
	"strconv"

	"github.com/go-chi/chi"
)

// WriteError :
func WriteError(w http.ResponseWriter, error string, code int) {
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.Header().Set("X-Content-Type-Options", "nosniff")
	w.WriteHeader(code)
	fmt.Fprintf(w, `{"message": %q}`, error)
}

// Status :
func Status(w http.ResponseWriter, r *http.Request) {
	code, err := strconv.Atoi(chi.URLParam(r, "status"))
	if err != nil {
		WriteError(w, err.Error(), 400)
		return
	}
	if code < 200 || code > 999 { // on status code, 1xx are also ok, but...
		WriteError(w, fmt.Sprintf("invalid WriteHeader code %v", code), 400)
		return
	}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(code)

	message := http.StatusText(code)
	fmt.Fprintf(w, `{"status": %d, "message": %q}`, code, message)
}
