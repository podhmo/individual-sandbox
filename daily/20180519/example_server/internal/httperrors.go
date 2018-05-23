package internal

import "net/http"

// BadRequest :
func BadRequest(w http.ResponseWriter, r *http.Request) {
	http.Error(w, "400 bad request", http.StatusBadRequest)
}

// InternalServerError :
func InternalServerError(w http.ResponseWriter, r *http.Request) {
	http.Error(w, "500 ISE", http.StatusInternalServerError)
}
