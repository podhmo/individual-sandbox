package main

import (
	"encoding/json"
	"log"
	"net/http"
	"net/url"
	"strings"

	"github.com/k0kubun/pp"
)

func run() error {
	values := url.Values{}
	values.Add("hello", "world")
	req, err := http.NewRequest("POST", "https://httpbin.org/post", strings.NewReader(values.Encode()))
	req.Header.Add("Content-Type", "application/x-www-form-urlencoded")
	response, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}
	defer response.Body.Close()
	var ob interface{}
	decoder := json.NewDecoder(response.Body)
	if err := decoder.Decode(&ob); err != nil {
		return err
	}
	pp.Println(ob)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
