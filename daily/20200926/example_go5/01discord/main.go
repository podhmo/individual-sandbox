package main

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"strings"
)

func DecodeJSON(r io.Reader, v interface{}) error {
	defer io.Copy(ioutil.Discard, r)
	return json.NewDecoder(r).Decode(v)
}

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

func main() {
	r := strings.NewReader(`{"title": "hello"}`)
	var todo Todo
	fmt.Println(DecodeJSON(r, &todo), todo)
	fmt.Println(DecodeJSON(r, &todo), todo)
}
