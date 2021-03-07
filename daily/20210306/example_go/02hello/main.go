package main

import (
	"fmt"
	"net/http"
	"net/url"
	"time"

	"github.com/podhmo/tenuki"
)

func main() {
	client := &http.Client{
		Timeout: 3 * time.Second,
	}

	t := tenuki.DefaultConfig().NewCaptureTransport("")
	_ = t
	client.Transport = t
	// fmt.Println("@", client, t)
	form := url.Values{}
	form.Add("freeform", "xxxxxxxxxxxxxxx")
	res, err := client.PostForm("https://httpbin.org/response-headers?foo=bar", form)
	_ = res
	_ = err
	// fmt.Println(res, err)
	fmt.Println("-")
	// client.Get("https://httpbin.org")
}
