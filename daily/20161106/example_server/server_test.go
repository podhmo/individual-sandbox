package main

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestHello(t *testing.T) {
	mux := NewHandler()
	ts := httptest.NewServer(mux)
	response, err := http.Get(ts.URL)
	if err != nil {
		t.Fatal(err)
	}
	defer response.Body.Close()
	b, err := ioutil.ReadAll(response.Body)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != "hello world" {
		t.Errorf("body: expected=%q, actual=%q", "hello world", string(b))
	}
}

func TestHelloHandler(t *testing.T) {
	req := httptest.NewRequest("GET", "http://localhost:8080/", nil)
	w := httptest.NewRecorder()

	targetAPI := Hello

	targetAPI(w, req)

	b, err := ioutil.ReadAll(w.Body)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != "hello world" {
		t.Errorf("body: expected=%q, actual=%q", "hello world", string(b))
	}
}
