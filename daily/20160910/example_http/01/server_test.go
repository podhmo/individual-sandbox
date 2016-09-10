package server

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestHelloHandler(t *testing.T) {
	targetHandler := HelloJSON

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
	fmt.Println(res)
}
