package main

import (
	"encoding/json"
	"os"

	"gopkg.in/guregu/null.v4"
)

type S struct {
	Name              null.String `json:"name"`
	NameWithOmitEmpty null.String `json:"name-with-omitempty,omitempty"` // bug (not supported)
}

func main() {
	{
		json.NewEncoder(os.Stdout).Encode(S{})
	}
	{
		json.NewEncoder(os.Stdout).Encode(S{
			Name:              null.StringFrom("Foo"),
			NameWithOmitEmpty: null.StringFrom("Foo"),
		})
	}
}
