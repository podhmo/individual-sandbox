package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"strings"
)

type S struct {
	Name string `json:"name" required:"true"`
	Age  int    `json:"age"`
}

func (s *S) UnmarshalJSON(b []byte) error {
	if err := requiredCheck(b, "name"); err != nil {
		return err
	}
	type t S
	return json.Unmarshal(b, (*t)(s))
}

func requiredCheck(b []byte, required ...string) error {
	buf := bytes.NewBuffer(b)

	dec := json.NewDecoder(buf)
	if tok, err := dec.Token(); err != nil || tok != json.Delim('{') {
		return fmt.Errorf("must be { %w", err)
	}

	var keys []string
	var stack []json.Delim
	for dec.More() || len(stack) > 0 {
		k, err := dec.Token()
		if err != nil {
			return fmt.Errorf("key: %q %w", k, err)
		}
		if len(stack) > 0 {
			if stack[len(stack)-1] == k {
				stack = stack[:len(stack)-1]
			}
			continue
		}

		keys = append(keys, k.(string))
		v, err := dec.Token()
		if err != nil {
			return fmt.Errorf("val: %q %w", v, err)
		}

		switch v {
		case json.Delim('{'):
			stack = append(stack, json.Delim('}'))
		case json.Delim('['):
			stack = append(stack, json.Delim(']'))
		default:
			log.Println("pairs", k, v)
		}
	}

	for _, want := range required {
		found := false
		for _, got := range keys {
			if want == got {
				found = true
				break
			}
		}
		if !found {
			return fmt.Errorf("required field %q is not found", want)
		}
	}
	if tok, err := dec.Token(); err != nil || tok != json.Delim('}') {
		return fmt.Errorf("must be { %w", err)
	}
	return nil
}

func main() {
	{
		code := `{"name": "foo", "age": 20}`
		fmt.Println(code)
		dec := json.NewDecoder(strings.NewReader(code))
		dec.DisallowUnknownFields()
		var ob S
		if err := dec.Decode(&ob); err != nil {
			panic(err)
		}
		fmt.Printf("%+v\n", ob)
	}
	fmt.Println("----------------------------------------")
	{
		code := `{"age": 20, "xxx": [{"name": "foo"}], "name": "x"}`
		fmt.Println(code)
		dec := json.NewDecoder(strings.NewReader(code))
		dec.DisallowUnknownFields()
		var ob S
		if err := dec.Decode(&ob); err != nil {
			panic(err)
		}
		fmt.Printf("%+v\n", ob)
	}
	fmt.Println("----------------------------------------")
	{
		code := `{"age": 20, "xxx": [{"name": "foo"}]}`
		fmt.Println(code)
		dec := json.NewDecoder(strings.NewReader(code))
		dec.DisallowUnknownFields()
		var ob S
		if err := dec.Decode(&ob); err != nil {
			panic(err)
		}
		fmt.Printf("%+v\n", ob)
	}
}
