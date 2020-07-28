package main

import (
	"bytes"
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestServer(t *testing.T) {
	ts := httptest.NewServer(NewHandler())
	defer ts.Close()

	res, err := http.Get(fmt.Sprintf("%s/hello", ts.URL))
	if err != nil {
		t.Fatal(err)
	}
	if res.StatusCode != 200 {
		t.Fatalf("want 200, but got %d", res.StatusCode)
	}
	var b bytes.Buffer
	if _, err := (&b).ReadFrom(res.Body); err != nil {
		t.Fatal(err)
	}
	defer res.Body.Close()

	if want := `{"message": "hello world"}`; b.String() != want {
		t.Errorf("want %s, but %s", want, b.String())
	}
}
