package main

import (
	"testing"

	"github.com/k0kubun/pp"
	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/try"
	"github.com/podhmo/noerror"
)

func TestAPI(t *testing.T) {
	h := Handler()
	c := webtest.NewClientFromHandler(h)

	type response struct {
		Now string `json:"now"`
	}
	var got response
	var want interface{}

	try.It{
		Code: 200,
		Want: &want,
		ModifyResponse: func(res webtest.Response) interface{} {
			noerror.Must(t, res.ParseJSONData(&got))
			pp.Println("previous", want)
			pp.Println("current", got)
			got.Now = want.(map[string]interface{})["now"].(string)
			// nowを何か良い感じに扱うか無視する
			return got
		},
	}.With(t, c,
		"GET", "/now",
	)
}
