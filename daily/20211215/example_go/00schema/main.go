package main

import (
	"encoding/json"
	"os"
	. "m/def"
)

func main() {
	var r []*Schema
	for _, ob := range []interface{}{Foo{}, Bar{}} {
		r = append(r, NewSchema(ob))
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(r)

}
