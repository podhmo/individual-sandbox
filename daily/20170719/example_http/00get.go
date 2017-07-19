package main

import (
	"encoding/json"
	"log"
	"net/http"

	"github.com/k0kubun/pp"
)

func run() error {
	response, err := http.Get("https://httpbin.org/anything")
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
