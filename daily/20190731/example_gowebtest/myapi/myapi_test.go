package myapi_test

import (
	"encoding/json"
	"fmt"
	"myapi/myapitest"
	"net/http"
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
	client := myapitest.NewClientForRecorder(
		myapitest.NewTestHandler(),
		myapitest.WithBasePath("/status"),
	)

	t.Run("200", func(t *testing.T) {
		got, teardown := client.Get("/200")
		defer teardown()

		if got.StatusCode() != 200 {
			t.Fatalf("status expect 200, but %d\n response: %s", got.StatusCode(), got.LazyBodyString())
		}

		data := map[string]interface{}{}
		if err := got.ParseData(&data); err != nil {
			t.Fatalf("parse error %+v\n response:%s", err, got.LazyBodyString()) // todo: show ???
		}

		// todo: assertion response
		fmt.Printf("body: %#+v", data)

		// todo: assertion db check
	})
}
