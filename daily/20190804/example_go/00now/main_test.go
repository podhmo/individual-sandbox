package main

import (
	"fmt"
	"net/http/httptest"
	"testing"

	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/middlewares"
	"github.com/podhmo/noerror"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(Handler())
	defer ts.Close()

	client := webtest.NewClientFromTestServer(
		ts,
		middlewares.ExpectStatusCode(200),
		middlewares.TakeSnapshot(),
	)
	got, err, teardown := client.Do(t, "/")
	noerror.Must(t, err)
	defer teardown()
	fmt.Println(got.LazyBodyString())
}
