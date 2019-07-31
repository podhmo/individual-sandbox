package myapi_test

import (
	"encoding/json"
	"fmt"
	"myapi/myapitest"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestIt(t *testing.T) {
	ts, teardown := myapitest.NewTestAPIServer()
	defer teardown()
	basepath := "/status"

	t.Run("200", func(t *testing.T) {
		res, err := http.Get(fmt.Sprintf("%s%s/200", ts.URL, basepath))
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
	handler := myapitest.NewTestHandler()
	basepath := "/status"

	t.Run("200", func(t *testing.T) {
		req := httptest.NewRequest("GET", fmt.Sprintf("%s/200", basepath), nil)
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
