package main

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strings"
	"testing"
)

func TestHello(t *testing.T) {
	mux := NewHandler()
	ts := httptest.NewServer(mux)
	defer ts.Close()
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

func TestEcho(t *testing.T) {
	mux := NewHandler()
	ts := httptest.NewServer(mux)
	defer ts.Close()
	response, err := http.PostForm(ts.URL+"/echo", url.Values{"message": {"hello"}})
	if err != nil {
		t.Fatal(err)
	}
	defer response.Body.Close()
	b, err := ioutil.ReadAll(response.Body)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != "hello" {
		t.Errorf("body: expected=%q, actual=%q", "hello", string(b))
	}
}

func TestEcho2(t *testing.T) {
	mux := NewHandler()
	ts := httptest.NewServer(mux)
	defer ts.Close()
	payload, err := json.Marshal(struct {
		Message string `json:"message"`
	}{Message: "hello"})
	if err != nil {
		t.Fatal(err)
	}
	response, err := http.Post(ts.URL+"/echo2", "application/json", bytes.NewBuffer(payload))

	if err != nil {
		t.Fatal(err)
	}
	defer response.Body.Close()
	b, err := ioutil.ReadAll(response.Body)
	if err != nil {
		t.Fatal(err)
	}
	if strings.TrimRight(string(b), "\n") != strings.TrimRight(string(payload), "\n") {
		t.Errorf("body: expected=%q, actual=%q", string(payload), string(b))
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

func TestEchoHandler(t *testing.T) {
	r := strings.NewReader(url.Values{"message": {"hello"}}.Encode())
	req := httptest.NewRequest("POST", "http://localhost:8080/echo", r)
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	w := httptest.NewRecorder()

	targetAPI := Echo

	targetAPI(w, req)

	b, err := ioutil.ReadAll(w.Body)
	if err != nil {
		t.Fatal(err)
	}
	if string(b) != "hello" {
		t.Errorf("body: expected=%q, actual=%q", "hello", string(b))
	}
}

func TestEcho2Handler(t *testing.T) {
	type B struct {
		Message string `json:"message"`
	}
	var buf bytes.Buffer
	body := &B{Message: "hello"}
	if err := json.NewEncoder(&buf).Encode(&body); err != nil {
		t.Fatal(err)
	}

	req := httptest.NewRequest("POST", "http://localhost:8080/echo2", &buf)
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	targetAPI := Echo2

	targetAPI(w, req)
	var body2 B
	if err := json.NewDecoder(w.Body).Decode(&body2); err != nil {
		t.Fatal(err)
	}
	if body2.Message != "hello" {
		t.Errorf("body: expected=%q, actual=%q", "hello", body2.Message)
	}
}
