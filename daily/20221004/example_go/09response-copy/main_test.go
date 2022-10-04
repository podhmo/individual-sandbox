package main

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"testing"
	"time"
)

type withWriter struct {
	http.ResponseWriter
	w io.Writer
}

func (w *withWriter) Write(b []byte) (int, error) {
	w.w.Write(b)
	return w.ResponseWriter.Write(b)
}

func W(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		if t, ok := w.(interface{ CloseNotify() <-chan bool }); ok {
			buf := new(bytes.Buffer)
			ch := t.CloseNotify()
			w = &withWriter{ResponseWriter: w, w: buf}
			if ch != nil {
				go func() {
					fmt.Fprintln(os.Stderr, "wait")
					<-ch
					fmt.Fprintln(os.Stderr, "start")
					fmt.Fprintln(os.Stderr, "-----------------------------------------")
					io.Copy(os.Stderr, buf)
					fmt.Fprintln(os.Stderr, "-----------------------------------------")
				}()
			}
		}
		next.ServeHTTP(w, req)
	})
}

func TestIt(t *testing.T) {
	ts := httptest.NewServer(W(http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		w.Header().Set("Content-Type", "application/jsonn")
		fmt.Fprintf(w, `{"message": "hello world"}`)
	})))
	defer ts.Close()
	res, err := http.Get(ts.URL)
	if err != nil {
		t.Fatalf("unexpected error: %+v", err)
	}
	if want, got := 200, res.StatusCode; want != got {
		t.Errorf("status code: want=%d != got=%d", want, got)
	}
	time.Sleep(1 * time.Second)
}
