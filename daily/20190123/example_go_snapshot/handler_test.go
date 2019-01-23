package handler_test

import (
	"bytes"
	"encoding/json"
	"handler"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestHello(t *testing.T) {
	rec := httptest.NewRecorder()
	srv := http.HandlerFunc(handler.Hello)

	var b bytes.Buffer
	encoder := json.NewEncoder(&b)
	if err := encoder.Encode(handler.HelloInput{Name: "w0rld"}); err != nil {
		t.Fatalf("on setup %q", err)
	}

	req := httptest.NewRequest("GET", "/hello", &b)
	srv.ServeHTTP(rec, req)

	resp := rec.Result()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected status is %d but %d", http.StatusOK, resp.StatusCode)
	}

	var got handler.HelloOutput
	decoder := json.NewDecoder(resp.Body)
	if err := decoder.Decode(&got); err != nil {
		t.Fatalf("unexpected response %q", err) // use tee reader for actural response?
	}
	defer resp.Body.Close()

	if expected := "hello w0rld"; expected != got.Message {
		t.Errorf("invalid message, expected=%q, but got=%q", expected, got.Message)
	}
}
