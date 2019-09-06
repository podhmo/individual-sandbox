package main

import (
	"net/http"
	"testing"

	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/hook"
	"github.com/podhmo/go-webtest/jsonequal"
	"github.com/podhmo/noerror"
)

func TestIt(t *testing.T) {
	client := webtest.NewClientFromHandler(http.HandlerFunc(handler))

	var want interface{}
	webtest.Try(t,
		func(t *testing.T, got webtest.Response) {
			noerror.Should(t,
				jsonequal.ShouldBeSame(
					jsonequal.From(got.JSONData()),
					jsonequal.From(want),
				),
			)
		}).With(
		client.GET("/",
			hook.ExpectCode(t, 200),
			hook.GetExpectedDataFromSnapshot(t, &want),
		),
	)
}

func TestIt2(t *testing.T) {
	client := webtest.NewClientFromHandler(http.HandlerFunc(handler))

	var want interface{}
	got, err, teardown := client.GET("/",
		hook.ExpectCode(t, 200),
		hook.GetExpectedDataFromSnapshot(t, &want),
	)
	noerror.Must(t, err)
	defer teardown()

	noerror.Should(t,
		jsonequal.ShouldBeSame(
			jsonequal.From(got.JSONData()),
			jsonequal.From(want),
		),
	)
}
