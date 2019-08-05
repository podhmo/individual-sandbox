package main

import (
	"net/http/httptest"
	"testing"
	"time"

	rfc3339 "github.com/podhmo/go-rfc3339"
	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/jsonequal"
	"github.com/podhmo/go-webtest/middlewares"
	"github.com/podhmo/go-webtest/snapshot"
	"github.com/podhmo/noerror"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(Handler())
	defer ts.Close()

	now := rfc3339.Format(time.Now())
	var want interface{}
	client := webtest.NewClientFromTestServer(
		ts,
		middlewares.ExpectStatusCode(200),
		middlewares.SnapshotTesting(
			&want,
			snapshot.WithReplaceMap(map[string]interface{}{
				"#/response/data/now": now,
			})),
	)
	got, err, teardown := client.Do(t, "/")
	noerror.Must(t, err)
	defer teardown()

	noerror.Should(t,
		jsonequal.ShouldBeSame(
			jsonequal.FromRawWithBytes(got.JSONData(), got.Body()),
			jsonequal.FromRaw(want),
		),
	)
}
