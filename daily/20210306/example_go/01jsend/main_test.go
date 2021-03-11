package main

import (
	"fmt"
	"strings"
	"testing"

	"github.com/podhmo/tenuki"
	"github.com/podhmo/tenuki/difftest"
)

// func init() {
// 	tenuki.DefaultLayout = capture.OpenAPILayout
// }

func TestSuccess(t *testing.T) {
	targetHandler := SuccessHandler

	f := tenuki.New(t)
	req := f.NewRequest("GET", "", nil)
	res := f.DoHandlerFunc(targetHandler, req,
		tenuki.AssertStatus(200),
	)

	want := `
{
    "status" : "success",
    "data" : {
        "posts" : [
            { "id" : 1, "title" : "A blog post", "body" : "Some useful content" },
            { "id" : 2, "title" : "Another blog post", "body" : "More content" }
        ]
     }
}`
	var got interface{}
	f.Extract().JSON(res, &got)
	difftest.AssertGotAndWantString(t, got, want)
}

func TestFail(t *testing.T) {
	targetHandler := FailHandler

	f := tenuki.New(t)
	req := f.NewJSONRequest("POST", "/articles", strings.NewReader(`{"content": "Some useful content"}`))
	res := f.DoHandlerFunc(targetHandler, req,
		tenuki.AssertStatus(400),
	)

	want := `
{
    "status" : "fail",
    "message" : "bad request",
    "data" : {
        "title": "A title is required"
     }
}`
	var got interface{}
	f.Extract().JSON(res, &got)
	difftest.AssertGotAndWantString(t, got, want)
}

func TestError(t *testing.T) {
	targetHandler := ErrorHandler

	f := tenuki.New(t)
	req := f.NewRequest("GET", "", nil)
	res := f.DoHandlerFunc(targetHandler, req,
		tenuki.AssertStatus(500),
	)

	want := `
{
    "status" : "error",
    "message" : "Unable to communicate with database"
}`
	var got interface{}
	f.Extract().JSON(res, &got)
	difftest.AssertGotAndWantString(t, got, want)
}

func TestNetworkUnreached(t *testing.T) {
	f := tenuki.New(t)
	req := f.NewRequest("GET", "xxx://localhost.xxx", nil)
	f.Do(req,
		tenuki.AssertError(func(err error) error {
			if err == nil {
				return fmt.Errorf("something wrong !!!!!!!!!!!!")
			}
			t.Logf("ok error is occured %q", err)
			return nil
		}),
	)
}
