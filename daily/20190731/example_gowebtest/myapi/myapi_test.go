package myapi

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(Handler())
	defer ts.Close()

	t.Run("200", func(t *testing.T) {
		res, err := http.Get(fmt.Sprintf("%s/200", ts.URL))
		if err != nil {
			t.Fatalf("%v", err) // todo: show response
		}

		data := map[string]interface{}{}
		decoder := json.NewDecoder(res.Body)
		if err := decoder.Decode(&data); err != nil {
			t.Fatalf("%v", err) // todo: show ???
		}
		defer res.Body.Close()

		// todo: assertion response
		fmt.Printf("body: %+v", data)

		// todo: assertion db check
	})
}

func TestUnit(t *testing.T) {
	handler := Handler().ServeHTTP
	t.Run("200", func(t *testing.T) {
		req := httptest.NewRequest("GET", "/200", nil)
		w := httptest.NewRecorder()
		handler(w, req)

		res := w.Result()

		data := map[string]interface{}{}
		decoder := json.NewDecoder(res.Body)
		if err := decoder.Decode(&data); err != nil {
			t.Fatalf("%v", err) // todo: show ???
		}
		defer res.Body.Close()

		// todo: assertion response
		fmt.Printf("body: %+v", data)

		// todo: assertion db check
	})
}
