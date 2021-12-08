package main

import (
	"bytes"
	"encoding/json"
	"fmt"

	"gopkg.in/guregu/null.v4"
)

type S struct {
	Name              null.String `json:"name"`
	NameWithOmitEmpty null.String `json:"name-with-omitempty,omitempty"` // bug (not supported)
}

func main() {
	{
		var ob S
		source := `{}`
		json.NewDecoder(bytes.NewBufferString(source)).Decode(&ob)
		fmt.Printf("input: %s\n", source)
		fmt.Printf("\tdecoded: %+v\n", ob)
	}
	{
		var ob S
		source := `{"name": null, "name-with-omitempty": null}`
		json.NewDecoder(bytes.NewBufferString(source)).Decode(&ob)
		fmt.Printf("input: %s\n", source)
		fmt.Printf("\tdecoded: %+v\n", ob)
	}
	{
		var ob S
		source := `{"name": "Foo", "name-with-omitempty": "Foo"}`
		json.NewDecoder(bytes.NewBufferString(source)).Decode(&ob)
		fmt.Printf("input: %s\n", source)
		fmt.Printf("\tdecoded: %+v\n", ob)
	}
}
