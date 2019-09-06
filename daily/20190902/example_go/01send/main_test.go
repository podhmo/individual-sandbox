package main

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/hook"
	"github.com/podhmo/noerror"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(Router())
	client := webtest.NewClientFromTestServer(ts)

	t.Run("401", func(t *testing.T) {
		got, err, teardown := client.GET(t, "/auth/basic-auth/user/pass",
			hook.ExpectCode(401),
		)
		noerror.Must(t, err)
		defer teardown()
		fmt.Println(got.Text())
	})

	t.Run("200", func(t *testing.T) {
		got, err, teardown := client.GET(t, "/auth/basic-auth/user/pass",
			hook.ExpectCode(200),
			webtest.WithModifyRequest(func(req *http.Request) {
				req.SetBasicAuth("user", "pass")
			}),
		)
		noerror.Must(t, err)
		defer teardown()
		fmt.Println(got.Text())
	})
}
