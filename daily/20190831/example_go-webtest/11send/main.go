package main

import (
	"fmt"

	"github.com/k0kubun/pp"
	webtest "github.com/podhmo/go-webtest"
)

func main() {
	url := "https://httpbin.org/"
	c := webtest.NewClientFromURL(url)

	got, err, teardown := c.GET("/get",
		webtest.WithQuery(webtest.MustParseQuery("xxx=yyy")),
	)
	if err != nil {
		panic(err)
	}
	defer teardown()
	pp.Println(got.Response().Header)
	fmt.Println(got.Text())
}
