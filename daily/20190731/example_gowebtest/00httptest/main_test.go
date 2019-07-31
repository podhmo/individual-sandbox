package main_test

import (
	"encoding/json"
	"fmt"
	"m/myapi"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(myapi.Handler())
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
