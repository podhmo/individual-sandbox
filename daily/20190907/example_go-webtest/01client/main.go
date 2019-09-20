package main

import (
	"fmt"
	"io"
	"net/http"
	"os"

	"github.com/k0kubun/pp"
	"github.com/podhmo/go-webtest/tripperware"
)

func main() {
	c := &http.Client{}

	{
		c := tripperware.DebugTrace().DecorateClient(c, true)
		res, err := c.Get("https://httpbin.org/get")
		pp.Println(err)
		defer res.Body.Close()
		io.Copy(os.Stdout, res.Body)
	}

	fmt.Println("----------------------------------------")
	res, err := c.Get("https://httpbin.org/get")
	pp.Println(err)
	defer res.Body.Close()
	io.Copy(os.Stdout, res.Body)
}
