package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"net/url"
	"time"

	"github.com/vvakame/go-harlog"
)

func main() {
	har := &harlog.Transport{}
	client := &http.Client{
		Timeout:   3 * time.Second,
		Transport: har,
	}

	form := url.Values{}
	form.Add("freeform", "xxxxxxxxxxxxxxx")
	res, err := client.PostForm("https://httpbin.org/response-headers", form)
	_ = res
	_ = err

	b, err := json.MarshalIndent(har.HAR(), "", "  ")
	if err != nil {
		log.Fatalf("!! %+v", err)
	}
	fmt.Println(string(b))
}
