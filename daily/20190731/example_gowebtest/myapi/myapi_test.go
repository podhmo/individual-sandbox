package myapi_test

import (
	"fmt"
	"myapi/myapitest"
	"testing"

	"github.com/podhmo/handy"
)

func TestIt(t *testing.T) {
	ts, teardown := myapitest.NewTestAPIServer()
	defer teardown()

	client := myapitest.NewClientForServer(
		ts,
		myapitest.WithBasePath("/status"),
	)

	t.Run("200", func(t *testing.T) {
		got, err, teardown := client.Get("/200")
		if err != nil {
			t.Fatalf("%+v", err) // add more contextual information?
		}
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

	t.Run("200,handy", func(t *testing.T) {
		got, err, teardown := client.Get("/200")

		handy.Must(t, handy.Equal(nil).Actual(err)) // add more contextual information?
		defer teardown()
		handy.Must(t,
			handy.Equal(200).Actual(got.StatusCode()).Describe("status"),
			"response:", got.LazyBodyString(),
		)

		data := map[string]interface{}{}
		handy.Must(t, handy.Equal(nil).Actual(got.ParseData(&data)).Describe("parse error"),
			"response:", got.LazyBodyString(),
		)

		// todo: assertion response
		fmt.Printf("body: %#+v", data)

		// todo: assertion db check
	})

	t.Run("200,handy,block", func(t *testing.T) {
		got, err, teardown := client.Get("/200")

		{
			handy.Must(t, handy.Equal(nil).Actual(err)) // add more contextual information?
			defer teardown()
			handy.Must(t,
				handy.Equal(200).Actual(got.StatusCode()).Describe("status"),
				"response:", got.LazyBodyString(),
			)
		}

		{

			data := map[string]interface{}{}
			handy.Must(t, handy.Equal(nil).Actual(got.ParseData(&data)).Describe("parse error"),
				"response:", got.LazyBodyString(),
			)

			// todo: assertion response
			fmt.Printf("body: %#+v", data)

			// todo: assertion db check
		}
	})
}

func TestUnit(t *testing.T) {
	client := myapitest.NewClientForRecorder(
		myapitest.NewTestHandler(),
		myapitest.WithBasePath("/status"),
	)

	t.Run("200", func(t *testing.T) {
		got, err, teardown := client.Get("/200")
		if err != nil {
			t.Fatalf("%+v", err) // add more contextual information?
		}
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
