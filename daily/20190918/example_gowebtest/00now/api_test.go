package main

import (
	"testing"
	"time"

	rfc3339 "github.com/podhmo/go-rfc3339"
	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/try"
)

func TestAPI(t *testing.T) {
	s := &Server{Now: func() time.Time {
		return rfc3339.MustParse("2000-01-01T00:00:10Z")
	}}
	h := s.Handler()
	c := webtest.NewClientFromHandler(h)

	var want interface{}
	try.It{
		Code: 200,
		Want: &want,
	}.With(t, c,
		"GET", "/now",
	)
}
