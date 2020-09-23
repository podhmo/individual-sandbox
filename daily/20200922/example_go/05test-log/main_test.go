package main

import (
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"testing"

	"github.com/go-chi/chi"
)

type testlogger struct {
	testing.TB
}

func (tl *testlogger) Printf(fmt string, args ...interface{}) {
	tl.Helper()
	tl.Logf("\x1b[90m"+fmt+"\x1b[0m", args...)
}

func TestIt(t *testing.T) {
	s := &Server{
		Router: chi.NewRouter(),
		logger: &testlogger{TB: t},
	}
	s.Mount()

	ts := httptest.NewServer(s)
	defer ts.Close()

	res, err := http.Get(fmt.Sprintf("%s/500", ts.URL))
	if err != nil {
		t.Fatalf("unexpected %v", err)
	}

	if res.StatusCode != 200 {
		t.Errorf("want 200 but %d", res.StatusCode)
	}

	fmt.Println("----------------------------------------")
	fmt.Print("response: ")
	io.Copy(os.Stderr, res.Body)
	fmt.Println("----------------------------------------")
	defer res.Body.Close()
}
