package main

import (
	"net/http"

	"github.com/k0kubun/pp"
	"github.com/podhmo/go-webtest/tripperware"
)

func main() {
	c := &http.Client{}
	pp.Println(c.Transport)
	{
		c := tripperware.DebugTrace().DecorateClient(c, true)
		pp.Println(c.Transport)
	}
	pp.Println(c.Transport)
}
