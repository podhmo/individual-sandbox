package main

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestHello(t *testing.T) {
	targetHandler := hello

	ts := httptest.NewServer(http.HandlerFunc(targetHandler))
	defer ts.Close()

	res, err := http.Get(ts.URL)
	if err != nil {
		t.Error(err)
		return
	}
	if res.StatusCode != 200 {
		t.Errorf("status code expected 200 but %q", res.StatusCode)
		return
	}
	b, err := ioutil.ReadAll(res.Body)
	if err != nil {
		t.Error(err)
		return
	}

	expected := "hello world"
	if string(b) != expected {
		t.Errorf("response body expected %q but %q", expected, string(b))
		return
	}
}
