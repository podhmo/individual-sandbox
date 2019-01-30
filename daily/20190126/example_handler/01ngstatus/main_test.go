package main_test

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// Handler :
func Handler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "hello")
	w.WriteHeader(http.StatusConflict)
}

func Test(t *testing.T) {
	handler := http.HandlerFunc(Handler)
	ts := httptest.NewServer(handler)
	defer ts.Close()

	res, err := http.Get(ts.URL)
	if err != nil {
		t.Fatal(err)
	}

	if want, got := http.StatusConflict, res.StatusCode; want != got {
		t.Fatalf("http status, want=%d, but got=%d", want, got)
	}

	{
		want := "hello"
		var b bytes.Buffer
		if _, err := io.Copy(&b, res.Body); err != nil {
			t.Fatal(err)
		}
		got := b.String()
		if !strings.Contains(got, want) {
			t.Errorf("body, must be including %s, but got %s", want, got)
		}
	}
}
