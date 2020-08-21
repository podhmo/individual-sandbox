package main

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
)

func Hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, `{"message": "hello"}`)
}

func TestIt(t *testing.T) {
	w := httptest.NewRecorder()
	r := httptest.NewRequest("GET", "/", nil)
	Hello(w, r)
}
