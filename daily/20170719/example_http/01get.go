package main

import (
	"encoding/json"
	"log"
	"net/http"

	"github.com/k0kubun/pp"
)

func run() error {
	req, err := http.NewRequest("GET", "https://httpbin.org/anything", nil)
	q := req.URL.Query()
	q.Add("hello", "world")
	q.Add("v", "1")
	q.Add("v", "2")
	req.URL.RawQuery = q.Encode()
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
