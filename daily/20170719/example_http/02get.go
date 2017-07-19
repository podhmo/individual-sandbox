package main

import (
	"encoding/json"
	"log"
	"net/http"

	"github.com/k0kubun/pp"
)

func run() error {
	req, err := http.NewRequest("GET", "https://httpbin.org/anything", nil)
	req.Header.Add("Authorization", "Bearer token-token-token")
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
