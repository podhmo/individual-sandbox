package main

import (
	"bytes"
	"encoding/json"
	"log"
	"net/http"

	"github.com/k0kubun/pp"
)

func run() error {
	buf := bytes.NewBufferString(`{"hello": "world"}`)
	req, err := http.NewRequest("POST", "https://httpbin.org/post", buf)
	req.Header.Add("Content-Type", "application/json")
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
