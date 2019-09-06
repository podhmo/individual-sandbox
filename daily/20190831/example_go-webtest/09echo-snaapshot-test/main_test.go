package main

import (
	"net/http"
	"strings"
	"testing"

	"github.com/labstack/echo"
	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/hook"
	"github.com/podhmo/go-webtest/jsonequal"
	"github.com/podhmo/noerror"
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

func TestCreateUser(t *testing.T) {
	// Setup
	client := webtest.NewClientFromHandler(setupHandler())

	var want interface{}
	userJSON := `{"name":"Jon Snow","email":"jon@labstack.com"}`
	got, err, teardown := client.POST("/users",
		webtest.WithJSON(strings.NewReader(userJSON)),
		hook.ExpectCode(t, 201),
		hook.GetExpectedDataFromSnapshot(t, &want),
	)

	// Assertions
	noerror.Must(t, err)
	defer teardown()
	noerror.Should(t,
		jsonequal.ShouldBeSame(
			jsonequal.FromRaw(want),
			jsonequal.FromRawWithBytes(got.JSONData(), got.Body()),
		),
	)
}

func TestGetUser(t *testing.T) {
	// Setup
	client := webtest.NewClientFromHandler(setupHandler())

	var want interface{}
	got, err, teardown := client.GET("/users/jon@labstack.com",
		hook.ExpectCode(t, 200),
		hook.GetExpectedDataFromSnapshot(t, &want),
	)

	// Assertions
	noerror.Must(t, err)
	defer teardown()
	noerror.Should(t,
		jsonequal.ShouldBeSame(
			jsonequal.FromRaw(want),
			jsonequal.FromRawWithBytes(got.JSONData(), got.Body()),
		),
	)
}
