package main

import (
	"testing"

	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/try"
	"github.com/podhmo/noerror"
)

func TestAPI(t *testing.T) {
	h := Handler()
	c := webtest.NewClientFromHandler(h)

	var got map[string]interface{}
	var want interface{}
	try.It{
		Code: 200,
		Want: &want,
		ModifyResponse: func(res webtest.Response) interface{} {
			noerror.Must(t, res.ParseJSONData(&got))
			delete(got, "now")
			delete(want.(map[string]interface{}), "now")
			return got
		},
	}.With(t, c,
		"GET", "/now",
	)
}
