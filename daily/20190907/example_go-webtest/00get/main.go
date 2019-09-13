package main

import (
	"fmt"

	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/tripperware"
)

func main() {
	c := webtest.NewClientFromURL("https://httpbin.org")
	res, err := c.Get("/get",
		webtest.WithTripperware(
			tripperware.DebugTrace(),
		),
	)
	if err != nil {
		panic(err)
	}
	fmt.Println(res.Text())
}
