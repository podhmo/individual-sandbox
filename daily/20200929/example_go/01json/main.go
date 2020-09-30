package main

import (
	"encoding/json"
	"fmt"
	"strings"
)

type Data = interface{}

type Object struct {
	Data
	Raw interface{} `json:"raw"`
}

type Object2 struct {
	Data interface{} `json:"data"`
	Raw  interface{} `json:"raw,omitempty"`
}

func (o *Object2) UnmarshalJSON(b []byte) error {
	// todo: slice, map
	d := map[string]interface{}{}
	if err := json.Unmarshal(b, &d); err != nil {
		return err
	}
	if o == nil {
		*o = Object2{}
	}
	if raw, ok := d["raw"]; ok {
		o.Raw = raw
		delete(d, "raw")
	}
	o.Data = d
	return nil
}

func main() {
	{
		decoder := json.NewDecoder(strings.NewReader(`
{"name": "foo", "raw": {"name": "foo", "age": 20}}
`))
		var ob Object
		decoder.Decode(&ob)
		fmt.Printf("%#+v\n", ob)
	}
	{
		decoder := json.NewDecoder(strings.NewReader(`
{"name": "foo", "raw": {"name": "foo", "age": 20}}
`))
		var ob Object2
		decoder.Decode(&ob)
		fmt.Printf("%#+v\n", ob)
	}
}
