package main

import (
	"bytes"
	"encoding/json"
	"io"
	"os"
	"strings"
)

type person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	// decode -> encode
	// r := bytes.NewBufferString(`{"name": "foo", "age": 20}`)
	r := strings.NewReader(`{"name": "foo", "age": 20}{"name": "bar", "age": 20}`)

	decoder := json.NewDecoder(r)

	// 直接出力したいだけならbytes.Buffer経由してio.Copy()する必要はない
	b := new(bytes.Buffer)
	encoder := json.NewEncoder(b)
	encoder.SetIndent("", "  ")

	for decoder.More() {
		var person person
		if err := decoder.Decode(&person); err != nil {
			panic(err)
		}
		// encode
		if err := encoder.Encode(person); err != nil {
			panic(err)
		}
		io.Copy(os.Stdout, b)
	}
}
