package main

import (
	"io"
	"net/http"
	"strings"
	"testing"

	"github.com/labstack/echo"
	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/hook"
	"github.com/podhmo/go-webtest/jsonequal"
	"github.com/podhmo/noerror"
)

var (
	userJSON = `{"name":"Jon Snow","email":"jon@labstack.com"}
`
)

func setupHandler() http.Handler {
	e := echo.New()
	mockDB := map[string]*User{
		"jon@labstack.com": &User{"Jon Snow", "jon@labstack.com"},
	}
	h := &handler{mockDB}

	e.GET("/users/:email", h.getUser)
	e.POST("/users", h.createUser)
	return e
}

func TestIT(t *testing.T) {

	cases := []struct {
		msg    string
		method string
		path   string
		code   int
		body   io.Reader
	}{
		{
			msg:    "POST /users 201",
			method: "POST",
			path:   "/users",
			code:   201,
			body:   strings.NewReader(userJSON),
		},
		{
			msg:    "GET /users/<email> 200",
			method: "GET",
			path:   "/users/jon@labstack.com",
			code:   200,
		},
	}

	// Setup
	client := webtest.NewClientFromHandler(setupHandler())

	for _, c := range cases {
		c := c
		t.Run(c.msg, func(t *testing.T) {
			var want interface{}

			options := []webtest.Option{
				hook.GetExpectedDataFromSnapshot(t, &want),
				hook.ExpectCode(t, c.code),
			}
			if c.body != nil {
				options = append(options, webtest.WithJSON(c.body))
			}
			got, err, teardown := client.Do(c.method, c.path, options...)

			// Assertions
			noerror.Must(t, err)
			defer teardown()

			noerror.Should(t,
				jsonequal.ShouldBeSame(
					jsonequal.FromRawWithBytes(got.JSONData(), got.Body()),
					jsonequal.FromRaw(want),
				),
			)
		})
	}
}
