package myapi

import (
	"fmt"
	"net/http"
)

// OK :
func OK(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	fmt.Fprintf(w, `{"message": 200}`)
}
