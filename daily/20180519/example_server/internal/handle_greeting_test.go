package internal

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestGreet(t *testing.T) {
	newRequest := func(t *testing.T, msg string) *http.Request {
		var buf bytes.Buffer
		buf.WriteString(msg)
		req, err := http.NewRequest(http.MethodPost, "/greet", &buf)
		if err != nil {
			t.Fatal("unexpected", err)
		}
		return req
	}

	srv := newServer()

	t.Run("no-json 404", func(t *testing.T) {
		req := newRequest(t, `{"name": "World"}`)

		w := httptest.NewRecorder()
		srv.ServeHTTP(w, req)

		res := w.Result()
		if res.StatusCode != http.StatusNotFound {
			t.Fatalf("expected status is 404 but %s", res.Status)
		}
	})

	t.Run("json 200", func(t *testing.T) {
		req := newRequest(t, `{"name": "World"}`)
		req.Header.Set("Content-Type", "/application/json")

		w := httptest.NewRecorder()
		srv.ServeHTTP(w, req)

		res := w.Result()
		if res.StatusCode != http.StatusOK {
			t.Fatalf("expected status is 200 but %s", res.Status)
		}

		m := map[string]string{}
		decoder := json.NewDecoder(res.Body)
		if err := decoder.Decode(&m); err != nil {
			t.Error("unexpected", err)
		}

		if m["message"] != "Hello World" {
			t.Error("invalid", m["message"])
		}
	})
}
