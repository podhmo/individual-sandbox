package main

import (
	"net/http"
	"testing"

	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/try"
)

func TestAPI(t *testing.T) {
	c := webtest.NewClientFromHandler(http.HandlerFunc(Add))
	var want interface{}
	try.It{
		Code: http.StatusOK,
		Want: &want,
	}.With(t, c, "GET", "/api/add?x=100&y=20")
}
