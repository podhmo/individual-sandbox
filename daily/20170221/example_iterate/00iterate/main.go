package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

// Point :
type Point struct {
	X int `json:"x"`
	Y int `json:"y"`
}

func main() {
	data := `
{
  "hits": {
    "totalHits": 2,
    "hits": [
      {"_source": {"x": 10, "y": 20}},
      {"_source": {"x": 11, "y": 21}}
    ]
  }
}
`
	var result SearchResult
	if err := json.Unmarshal([]byte(data), &result); err != nil {
		panic(err)
	}

	var typ Point
	for _, item := range result.Each(reflect.TypeOf(typ)) {
		if p, ok := item.(Point); ok {
			fmt.Println("x", p.X, "y", p.Y)
		}
	}
}
