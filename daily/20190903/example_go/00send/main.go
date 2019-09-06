package main

import (
	"bytes"
	"encoding/json"
	"io"
	"log"
	"net/http"
	"os"
)

type Person struct {
	Name string `json:"name"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	var body bytes.Buffer
	encoder := json.NewEncoder(&body)
	if err := encoder.Encode(&Person{Name: "foo"}); err != nil {
		return err
	}
	req, err := http.NewRequest("POST", "https://httpbin.org/post", &body)
	req.Header.Set("Content-Type", "application/json")
	if err != nil {
		return err
	}
	res, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	io.Copy(os.Stdout, res.Body)

	return nil
}
