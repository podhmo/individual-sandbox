package main

import (
	"encoding/json"
	"net/http"

	"github.com/davecgh/go-spew/spew"
)

// Result :
type Result struct {
	URL       string `json:"url"`
	Title     string `json:"title"`
	UpdatedAt string `json:"updated_at"`
}

func get() error {
	resp, err := http.Get("https://qiita.com/api/v1/search?q=go")
	if err != nil {
		return err
	}
	decoder := json.NewDecoder(resp.Body)
	var xs []Result
	if err := decoder.Decode(&xs); err != nil {
		return err
	}
	spew.Dump(xs)
	defer resp.Body.Close()
	return nil
}

func main() {
	get()
}
