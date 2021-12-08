package handler

import (
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"strings"
	"testing"
)

func TestHandler(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(Handle))
	defer ts.Close()
	res, err := http.Post(ts.URL, "application/json", strings.NewReader(`{"name":"foo"}`))
	if err != nil {
		t.Errorf("unexpected error: %+v", err)
	}
	io.Copy(os.Stdout, res.Body)
	defer res.Body.Close()
}
