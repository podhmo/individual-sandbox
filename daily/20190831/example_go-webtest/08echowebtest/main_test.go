package main

import (
	"net/http"
	"strings"
	"testing"

	"github.com/labstack/echo"
	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/ex"
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

func TestCreateUser(t *testing.T) {
	// Setup
	client := webtest.NewClientFromHandler(setupHandler())

	got, err, teardown := client.Do(t, "/users",
		webtest.WithMethod("POST"),
		webtest.WithJSON(strings.NewReader(userJSON)),
		ex.ExpectCode(201),
	)

	// Assertions
	noerror.Must(t, err)
	defer teardown()
	noerror.Should(t, noerror.Equal(userJSON).Actual(got.Text()))
}

func TestGetUser(t *testing.T) {
	// Setup
	client := webtest.NewClientFromHandler(setupHandler())

	got, err, teardown := client.Do(t, "/users/jon@labstack.com",
		ex.ExpectCode(200),
	)

	// Assertions
	noerror.Must(t, err)
	defer teardown()
	noerror.Should(t, noerror.Equal(userJSON).Actual(got.Text()))
}
